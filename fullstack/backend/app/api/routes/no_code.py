import enum

from fastapi import APIRouter, Depends
from fastapi.exceptions import HTTPException

from app.db import Session, get_current_user, get_db
from app.models.no_code.canvas import NoCodeCanvas
from app.models.no_code.parameter import (
    DefaultValue,
    DisplayInfo,
    NoCodeParameter,
    NoCodeParameterGroup,
    NoCodeParameterOption,
    ParameterId,
    ParameterType,
    SelectOption,
)
from app.models.no_code.pipeline_step import NoCodePipelineStep
from app.models.no_code.tool import NoCodeTool, NoCodeToolParameter, ToolId
from app.models.no_code.widget import NoCodeWidget
from app.models.user import User

from app.no_code.decoration import make_tools
from app.no_code.functions import (
    main_render_loop,
)
from app.schemas.no_code import (
    NoCodeCanvasOut,
    NoCodeToolIn,
    NoCodeToolOut,
    NoCodeWidgetCreate,
    NoCodeWidgetIn,
    NoCodeWidgetOut,
    NoCodeWidgetUpdate,
    NoCodeParameterUpdate,
    Parameter,
    ParameterGroupOut,
)
from app.seed.accounts_page import seed_account_page

router = APIRouter(prefix="/no_code", tags=["no_code"])


@router.get("/get_no_code_tools", response_model=list[NoCodeToolOut])
def get_no_code_tools(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[NoCodeToolOut]:
    return make_tools()


def process_widget(
    *, session: Session, user: User, widget: NoCodeWidgetIn, parameters: list[Parameter]
) -> NoCodeWidgetOut:
    response = main_render_loop(widget.pipeline, session, user, parameters)

    return NoCodeWidgetOut(
        id=widget.id,
        parameters=response.parameters,
        name=widget.name,
        description=widget.description,
        result=response.result,
        result_type=response.result_type,
        col_span=widget.col_span,
        row_span=widget.row_span,
        col=widget.col,
        row=widget.row,
        type=widget.type,
    )


@router.post("/save_no_code_tool", response_model=list[NoCodeWidgetOut])
def save_no_code_tool(
    widgets: list[NoCodeWidgetIn],
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[NoCodeWidgetOut]:
    try:
        return [
            process_widget(session=session, user=user, widget=widget, parameters=[])
            for widget in widgets
        ]

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.post("/refetch_widget", response_model=NoCodeWidgetOut)
def refetch_widget(
    widget_id: int,
    parameters: list[Parameter] | None = None,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> NoCodeWidgetOut:
    widget = get_widget(session, user, widget_id)
    return process_widget(
        session=session, user=user, widget=widget, parameters=parameters or []
    )


@router.post("/update_parameter", response_model=dict[str, str])
def update_parameter(
    parameter_id: int,
    parameter_update: NoCodeParameterUpdate,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> dict[str, str]:
    parameter = (
        session.query(NoCodeParameter)
        .filter(NoCodeParameter.id == parameter_id, NoCodeParameter.user_id == user.id)
        .one_or_none()
    )
    if not parameter:
        raise HTTPException(status_code=400, detail="param does not exist")

    parameter.label = parameter_update.label
    parameter.display_info = DisplayInfo(
        views=parameter.display_info.views,
        size=parameter.display_info.size,
        row=parameter_update.row,
        row_span=parameter_update.row_span,
        col=parameter_update.col,
        col_span=parameter_update.col_span,
    )

    session.commit()

    return {"message": "updated parameter"}


@router.post("/create_widget", response_model=NoCodeWidgetOut)
def create_widget(
    widget: NoCodeWidgetCreate,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> None:
    pass


@router.post("/update_widget", response_model=dict[str, str])
def update_widget(
    widget_id: int,
    widget_update: NoCodeWidgetUpdate,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> dict[str, str]:
    widget = (
        session.query(NoCodeWidget)
        .filter(NoCodeWidget.id == widget_id, NoCodeWidget.user_id == user.id)
        .one_or_none()
    )
    if not widget:
        raise HTTPException(status_code=400, detail="widget does not exist")

    widget.col_span = widget_update.col_span
    widget.column = widget_update.col
    widget.row = widget_update.row
    widget.row_span = widget_update.row_span
    widget.name = widget_update.name

    session.commit()

    normalize_widget_locations(session, user, widget)

    return {"message": "updated widget"}


def normalize_widget_locations(
    session: Session, user: User, widget: NoCodeWidget
) -> None:
    widgets = (
        session.query(NoCodeWidget)
        .filter(
            NoCodeWidget.canvas_id == widget.canvas_id, NoCodeWidget.user_id == user.id
        )
        .all()
    )

    if not widgets:
        return

    min_row = min(w.row for w in widgets)
    min_col = min(w.column for w in widgets)

    row_shift = min_row - 1 if min_row > 1 else 0
    col_shift = min_col - 1 if min_col > 1 else 0

    for w in widgets:
        w.row -= row_shift
        w.column -= col_shift
        print(w.row, w.column)

    session.commit()


class PageVariant(str, enum.Enum):
    accounts = "account-page"


@router.post("/get_no_code_dashboard", response_model=NoCodeCanvasOut)
def get_no_code_dashboard(
    variant: PageVariant,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> NoCodeCanvasOut:
    return generate_canvas_for_slug(session, user, slug=variant.value)


def remove_parameters(
    widget_id: int,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> dict[str, str]:
    # Get all tools associated with this widget
    tools = (
        session.query(NoCodeTool)
        .filter(
            NoCodeTool.widget_id == widget_id,
            NoCodeTool.user_id == user.id,
        )
        .all()
    )

    tool_ids = [tool.id for tool in tools]

    tool_parameters = (
        session.query(NoCodeToolParameter)
        .filter(NoCodeToolParameter.tool_id.in_(tool_ids))
        .all()
    )

    parameter_ids = [tp.parameter_id for tp in tool_parameters]

    session.query(NoCodeParameterOption).filter(
        NoCodeParameterOption.parameter_id.in_(parameter_ids)
    ).delete(synchronize_session=False)

    session.query(NoCodeParameter).filter(NoCodeParameter.id.in_(parameter_ids)).delete(
        synchronize_session=False
    )

    session.commit()

    return {"message": "removed parameters"}


@router.post("/remove_widget", response_model=dict[str, str])
def remove_widget(
    widget_id: int,
    canvas_id: int,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> dict[str, str]:
    widget = (
        session.query(NoCodeWidget)
        .join(NoCodeCanvas, NoCodeCanvas.id == NoCodeWidget.canvas_id)
        .filter(
            NoCodeWidget.id == widget_id,
            NoCodeCanvas.id == canvas_id,
            NoCodeWidget.user_id == user.id,
        )
        .one_or_none()
    )
    if not widget:
        raise HTTPException(status_code=400, detail="widget does not exist")

    # First delete tool parameters
    tool_ids = (
        session.query(NoCodeTool.id)
        .filter(
            NoCodeTool.widget_id == widget_id,
            NoCodeTool.user_id == user.id,
            NoCodeTool.canvas_id == canvas_id,
        )
        .all()
    )
    tool_ids = [t[0] for t in tool_ids]

    if tool_ids:
        # Delete tool parameters first
        session.query(NoCodeToolParameter).filter(
            NoCodeToolParameter.tool_id.in_(tool_ids)
        ).delete(synchronize_session=False)

        # Then delete pipeline steps
        session.query(NoCodePipelineStep).filter(
            NoCodePipelineStep.tool_id.in_(tool_ids)
        ).delete(synchronize_session=False)

        # Finally delete the tools
        session.query(NoCodeTool).filter(NoCodeTool.id.in_(tool_ids)).delete(
            synchronize_session=False
        )

    remove_parameters(widget_id, user, session)

    session.delete(widget)
    session.commit()

    return {"message": "removed widget"}


def create_seed_page(session: Session, user: User) -> NoCodeCanvas:
    return seed_account_page(user.id, session)


def generate_canvas_for_slug(
    session: Session, user: User, slug: str
) -> NoCodeCanvasOut:
    db_canvas = (
        session.query(NoCodeCanvas).filter_by(user_id=user.id, slug=slug).one_or_none()
    )
    if not db_canvas:
        db_canvas = create_seed_page(session, user)

    db_widgets = session.query(NoCodeWidget).filter_by(canvas_id=db_canvas.id).all()

    widget_ids = [w.id for w in db_widgets]

    db_param_groups = (
        session.query(NoCodeParameterGroup).filter_by(canvas_id=db_canvas.id).all()
    )

    db_parameters = (
        session.query(NoCodeParameter)
        .filter(NoCodeParameter.group_id.in_([pg.id for pg in db_param_groups]))
        .all()
    )
    param_id_to_param: dict[ParameterId, NoCodeParameter] = {
        p.id: p for p in db_parameters
    }

    param_id_to_options: dict[int, list[SelectOption]] = {}
    if db_parameters:
        db_options = (
            session.query(NoCodeParameterOption)
            .filter(
                NoCodeParameterOption.parameter_id.in_([p.id for p in db_parameters])
            )
            .all()
        )
        for opt in db_options:
            param_id_to_options.setdefault(opt.parameter_id, []).append(
                SelectOption(key=opt.key, value=opt.value)
            )

    db_tools = (
        session.query(NoCodeTool).filter(NoCodeTool.canvas_id == db_canvas.id).all()
    )
    tool_id_to_tool = {tool.id: tool for tool in db_tools}

    db_pipeline_steps = (
        session.query(NoCodePipelineStep)
        .filter(NoCodePipelineStep.widget_id.in_(widget_ids))
        .order_by(NoCodePipelineStep.order_index)
        .all()
    )

    db_tool_parameters = (
        session.query(NoCodeToolParameter)
        .filter(NoCodeToolParameter.tool_id.in_([tool.id for tool in db_tools]))
        .all()
    )
    tool_id_to_param_ids: dict[ToolId, list[ParameterId]] = {}
    for link in db_tool_parameters:
        tool_id_to_param_ids.setdefault(link.tool_id, []).append(link.parameter_id)

    widgets_out = []
    for widget in db_widgets:
        widget_steps = [
            step for step in db_pipeline_steps if step.widget_id == widget.id
        ]
        pipeline = []

        for step in widget_steps:
            tool = tool_id_to_tool[step.tool_id]
            param_ids = tool_id_to_param_ids.get(tool.id, [])

            tool_params_out = []
            for param_id in param_ids:
                param = param_id_to_param.get(param_id)
                if param:
                    default_value = param.default_value
                    if param.type in {
                        ParameterType.SELECT,
                        ParameterType.MULTI_SELECT,
                    } and param_id_to_options.get(param.id):
                        default_value = DefaultValue(
                            value=param_id_to_options[param.id][0]
                        )
                    tool_params_out.append(
                        Parameter(
                            id=param.id,
                            group_id=param.group_id,
                            name=param.name,
                            label=param.label,
                            type=param.type,
                            trigger_refetch=param.trigger_refetch,
                            dependent_widgets=param.dependent_widgets,
                            option_generator=param.option_generator_key,
                            options=param_id_to_options.get(param.id),
                            default_value=default_value,
                            display_info=param.display_info,
                        )
                    )

            pipeline.append(
                NoCodeToolIn(
                    tool=tool.tool_name,
                    parameters=tool_params_out,
                )
            )

        widgets_out.append(
            NoCodeWidgetIn(
                id=widget.id,
                name=widget.name,
                description=widget.label or "",
                row=widget.row,
                col=widget.column,
                row_span=widget.row_span,
                col_span=widget.col_span,
                type=widget.widget_type,
                pipeline=pipeline,
            )
        )

    parameter_groups_out = [
        ParameterGroupOut(
            id=group.id,
            type=group.group_type,
            name=group.name,
        )
        for group in db_param_groups
    ]

    parameters_out = []
    for param in db_parameters:
        default_value = param.default_value
        if param.type in {
            ParameterType.SELECT,
            ParameterType.MULTI_SELECT,
        } and param_id_to_options.get(param.id):
            default_value = DefaultValue(value=param_id_to_options[param.id][0])

        parameters_out.append(
            Parameter(
                id=param.id,
                group_id=param.group_id,
                name=param.name,
                label=param.label,
                type=param.type,
                trigger_refetch=param.trigger_refetch,
                dependent_widgets=param.dependent_widgets,
                option_generator=param.option_generator_key,
                options=param_id_to_options.get(param.id),
                default_value=default_value,
                display_info=param.display_info,
            )
        )

    return NoCodeCanvasOut(
        name=db_canvas.name,
        canvas_id=db_canvas.id,
        widgets=widgets_out,
        parameters=parameters_out,
        parameter_groups=parameter_groups_out,
    )


def get_widget(session: Session, user: User, widget_id: int) -> NoCodeWidgetIn:
    db_widget = (
        session.query(NoCodeWidget).filter_by(id=widget_id, user_id=user.id).first()
    )
    if not db_widget:
        raise HTTPException(status_code=404, detail="Widget not found")

    db_pipeline_steps = (
        session.query(NoCodePipelineStep)
        .filter_by(widget_id=widget_id)
        .order_by(NoCodePipelineStep.order_index)
        .all()
    )

    tool_ids = [step.tool_id for step in db_pipeline_steps]
    db_tools = session.query(NoCodeTool).filter(NoCodeTool.id.in_(tool_ids)).all()
    tool_id_to_tool = {tool.id: tool for tool in db_tools}

    db_parameters = session.query(NoCodeParameter).all()
    db_parameter_options = session.query(NoCodeParameterOption).all()

    param_id_to_param = {param.id: param for param in db_parameters}
    param_id_to_options: dict[ParameterId, list[SelectOption]] = {}
    for option in db_parameter_options:
        param_id_to_options.setdefault(option.parameter_id, []).append(
            SelectOption(key=option.key, value=option.value)
        )

    db_tool_parameters = (
        session.query(NoCodeToolParameter)
        .filter(NoCodeToolParameter.tool_id.in_(tool_ids))
        .all()
    )

    tool_id_to_param_ids: dict[ToolId, list[ParameterId]] = {}
    for link in db_tool_parameters:
        tool_id_to_param_ids.setdefault(link.tool_id, []).append(link.parameter_id)

    pipeline = []
    for step in db_pipeline_steps:
        tool = tool_id_to_tool[step.tool_id]

        param_ids = tool_id_to_param_ids.get(tool.id, [])
        tool_params = []
        for param_id in param_ids:
            db_param = param_id_to_param.get(param_id)
            if db_param:
                tool_params.append(
                    Parameter(
                        id=db_param.id,
                        group_id=db_param.group_id,
                        name=db_param.name,
                        label=db_param.label,
                        type=db_param.type,
                        default_value=db_param.default_value,
                        options=param_id_to_options.get(db_param.id),
                        trigger_refetch=db_param.trigger_refetch,
                        dependent_widgets=db_param.dependent_widgets,
                        option_generator=db_param.option_generator_key,
                        display_info=db_param.display_info,
                    )
                )

        pipeline.append(
            NoCodeToolIn(
                tool=tool.tool_name,
                parameters=tool_params,
            )
        )

    return NoCodeWidgetIn(
        id=db_widget.id,
        name=db_widget.name,
        description=db_widget.label or "",
        row=db_widget.row,
        col=db_widget.column,
        row_span=db_widget.row_span,
        col_span=db_widget.col_span,
        type=db_widget.widget_type,
        pipeline=pipeline,
    )
