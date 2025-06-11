from app.api.routes.no_code_pages.account_page import generate_account_page
from app.db import Session, get_db_for_user
from app.models.no_code.canvas import NoCodeCanvas
from app.models.no_code.parameter import (
    NoCodeParameter,
    NoCodeParameterGroup,
    NoCodeParameterOption,
)
from app.models.no_code.pipeline_step import NoCodePipelineStep
from app.models.no_code.tool import NoCodeTool, NoCodeToolParameter
from app.models.no_code.widget import NoCodeWidget
from app.models.user import User


def seed_account_page(user_id: int, session: Session | None = None) -> NoCodeCanvas:
    if not session:
        session = next(get_db_for_user(user_id))

    canvas = None
    user = session.query(User).filter(User.id == user_id).one()
    canvas_data = generate_account_page(session, user)

    canvas = NoCodeCanvas(
        name=canvas_data.name,
        slug=canvas_data.name.lower().replace(" ", "-"),
        user_id=user_id,
    )
    session.add(canvas)
    session.flush()

    widget_id_lookup = {}

    for widget_data in canvas_data.widgets:
        widget = NoCodeWidget(
            name=widget_data.name or "",
            widget_type=widget_data.type,
            label=widget_data.name,
            canvas_id=canvas.id,
            user_id=user_id,
            row=widget_data.row,
            column=widget_data.col,
            row_span=widget_data.row_span,
            col_span=widget_data.col_span,
        )
        session.add(widget)
        session.flush()
        widget_id_lookup[widget_data.id] = widget.id

    # Create Parameter Groups
    group_id_lookup: dict[int, int] = {}
    for group_data in canvas_data.parameter_groups:
        group = NoCodeParameterGroup(
            group_type=group_data.type,
            name=group_data.name,
            canvas_id=canvas.id,
            user_id=user_id,
        )
        session.add(group)
        session.flush()
        group_id_lookup[group_data.id] = group.id

    session.flush()

    # Create Parameters
    param_id_lookup = {}
    for param_data in canvas_data.parameters:
        param = NoCodeParameter(
            type=param_data.type,
            user_id=user_id,
            name=param_data.name,
            label=param_data.label or "",
            group_id=group_id_lookup[param_data.group_id],
            trigger_refetch=param_data.trigger_refetch,
            dependent_widgets=[
                widget_id_lookup[w] for w in param_data.dependent_widgets
            ],
            option_source_type="STATIC"
            if param_data.options
            else ("DYNAMIC" if param_data.option_generator else None),
            option_generator_key=param_data.option_generator,
            default_value=param_data.default_value,
            display_info=param_data.display_info,
        )
        print(repr(param))
        session.add(param)
        param_id_lookup[param_data.id] = param

    session.flush()

    for param_data in canvas_data.parameters:
        if param_data.options:
            for opt in param_data.options:
                option = NoCodeParameterOption(
                    parameter_id=param_id_lookup[param_data.id].id,
                    key=opt.key,
                    value=opt.value,
                )
                session.add(option)

    session.flush()
    for widget_data in canvas_data.widgets:
        for order_index, tool_data in enumerate(widget_data.pipeline or []):
            tool = NoCodeTool(
                user_id=user_id,
                widget_id=widget_id_lookup[widget_data.id],
                tool_name=tool_data.tool,
                canvas_id=canvas.id,
            )
            session.add(tool)
            session.flush()

            for param_ in tool_data.parameters or []:
                db_param = param_id_lookup.get(param_.id)
                if db_param:
                    tool_param = NoCodeToolParameter(
                        tool_id=tool.id,
                        parameter_id=db_param.id,
                    )
                    session.add(tool_param)
                else:
                    print(
                        f"⚠️  Warning: Could not find DB param for param.id={param_.id}"
                    )

            # Continue as before: make pipeline step
            step = NoCodePipelineStep(
                widget_id=widget_id_lookup[widget_data.id],
                tool_id=tool.id,
                order_index=order_index,
            )
            session.add(step)

    session.commit()
    print(f"✅ Successfully seeded account page for user {user_id}")

    session.close()
    if not canvas:
        raise ValueError("failed to generate no code page")
    return canvas


def delete_account_page(user_id: int) -> None:
    session = next(get_db_for_user(user_id))

    try:
        # Find the canvas for this user
        canvas = (
            session.query(NoCodeCanvas)
            .filter_by(user_id=user_id, slug="account-page")
            .first()
        )
        if not canvas:
            print(f"⚠️ No account page canvas found for user {user_id}")
            return

        # Delete dependent objects in the correct order

        # 1. Find all widgets for the canvas
        widgets = session.query(NoCodeWidget).filter_by(canvas_id=canvas.id).all()
        widget_ids = [w.id for w in widgets]

        # 2. Find all tools
        tools = (
            session.query(NoCodeTool).filter(NoCodeTool.canvas_id == canvas.id).all()
        )
        tool_ids = [t.id for t in tools]

        # 3. Delete pipeline steps first
        if tool_ids:
            session.query(NoCodePipelineStep).filter(
                NoCodePipelineStep.tool_id.in_(tool_ids)
            ).delete(synchronize_session=False)

        if tool_ids:
            session.query(NoCodeToolParameter).filter(
                NoCodeToolParameter.tool_id.in_(tool_ids)
            ).delete(synchronize_session=False)

        # 4. Delete tools
        if tools:
            session.query(NoCodeTool).filter(NoCodeTool.id.in_(tool_ids)).delete(
                synchronize_session=False
            )

        # 5. Delete widgets
        if widgets:
            session.query(NoCodeWidget).filter(NoCodeWidget.id.in_(widget_ids)).delete(
                synchronize_session=False
            )

        # 6. Delete parameters
        parameters = (
            session.query(NoCodeParameter)
            .join(NoCodeParameterGroup)
            .filter(NoCodeParameterGroup.canvas_id == canvas.id)
            .all()
        )
        parameter_ids = [p.id for p in parameters]

        if parameter_ids:
            session.query(NoCodeParameterOption).filter(
                NoCodeParameterOption.parameter_id.in_(parameter_ids)
            ).delete(synchronize_session=False)
            session.query(NoCodeToolParameter).filter(
                NoCodeToolParameter.parameter_id.in_(parameter_ids)
            ).delete(synchronize_session=False)

        # 7. Delete parameters
        if parameter_ids:
            session.query(NoCodeParameter).filter(
                NoCodeParameter.id.in_(parameter_ids)
            ).delete(synchronize_session=False)

        # 8. Delete parameter groups
        session.query(NoCodeParameterGroup).filter(
            NoCodeParameterGroup.canvas_id == canvas.id
        ).delete(synchronize_session=False)

        # 9. Finally, delete the canvas
        session.delete(canvas)

        session.commit()
        print(f"🧹 Successfully deleted account page for user {user_id}")

    except Exception as e:
        session.rollback()
        print(f"❌ Failed to delete account page: {e.__str__()}")
        raise

    finally:
        session.close()


if __name__ == "__main__":
    delete_account_page(1)
    seed_account_page(1)
