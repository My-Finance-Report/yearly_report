from datetime import datetime, timezone
from functools import partial

from app.db import Session
from app.models.no_code.parameter import (
    DefaultValue,
    DisplayInfo,
    DisplaySize,
    ParameterGroupId,
    ParameterGroupType,
    ParameterType,
    SelectOption,
)
from app.models.no_code.widget import WidgetId, WidgetType
from app.models.user import User

from app.no_code.functions import (
    extract_parameters_from_pipeline,
    make_account_choices,
)
from app.schemas.no_code import (
    NoCodeCanvasCreate,
    NoCodeToolIn,
    NoCodeTransaction,
    NoCodeWidgetIn,
    NoCodeWidgetIn,
    Parameter,
    ParameterGroupOut,
)


def get_shared_param(
    session: Session,
    user: User,
    widget_id: WidgetId,
    global_parameters: dict[str, Parameter],
) -> Parameter:
    param_name = "account_id"

    if param_name not in global_parameters:
        account_choices = make_account_choices(session, user)
        default_value = (
            DefaultValue(value=account_choices[0]) if len(account_choices) > 0 else None
        )
        new_param = Parameter(
            id=2,
            group_id=ParameterGroupId(0),
            name=param_name,
            label="",
            type=ParameterType.SELECT,
            options=account_choices,
            default_value=default_value,
            trigger_refetch=True,
            dependent_widgets=[widget_id],
            display_info=DisplayInfo(
                views=["page"],
                size=DisplaySize.LARGE,
                row=14,
                col=1,
                row_span=2,
                col_span=4,
            ),
        )
        global_parameters[param_name] = new_param
    else:
        param = global_parameters[param_name]
        param.dependent_widgets.append(widget_id)

    return global_parameters[param_name]


def first_n(
    session: Session,
    user: User,
    widget_id: WidgetId,
    global_parameters: dict[str, Parameter],
) -> NoCodeToolIn:
    return NoCodeToolIn(
        tool="first_n_transactions",
        parameters=[
            Parameter(
                id=1,
                name="n",
                group_id=ParameterGroupId(0),
                label="Transactions to display",
                type=ParameterType.SELECT,
                options=[
                    SelectOption(key=str(10), value=str(10)),
                    SelectOption(key=str(20), value=str(20)),
                    SelectOption(key=str(50), value=str(50)),
                    SelectOption(key=str(100), value=str(100)),
                ],
                default_value=DefaultValue(value=SelectOption(key="12", value="12")),
            ),
            get_shared_param(session, user, widget_id, global_parameters),
            Parameter(
                id=3,
                group_id=ParameterGroupId(0),
                name="page",
                label="",
                trigger_refetch=True,
                type=ParameterType.PAGINATION,
                options=[],
                dependent_widgets=[widget_id],
                option_generator="get_pages_per_account",
                default_value=DefaultValue(value=SelectOption(key="1", value="1")),
                display_info=DisplayInfo(
                    views=["page"],
                    row=44,
                    col=5,
                    row_span=1,
                    col_span=4,
                ),
            ),
        ],
    )


def most_recent_n(widget_id: WidgetId) -> NoCodeToolIn:
    return NoCodeToolIn(
        tool="first_n_transactions",
        parameters=[
            Parameter(
                id=4,
                name="n",
                group_id=ParameterGroupId(0),
                label="Transactions to display",
                type=ParameterType.SELECT,
                options=[
                    SelectOption(key=str(3), value=str(3)),
                    SelectOption(key=str(5), value=str(5)),
                    SelectOption(key=str(50), value=str(50)),
                    SelectOption(key=str(100), value=str(100)),
                ],
                default_value=DefaultValue(value=SelectOption(key="20", value="20")),
                display_info=None,
            ),
            Parameter(
                id=108,
                name="search_string",
                group_id=ParameterGroupId(0),
                label="Search Transactions",
                type=ParameterType.STRING,
                trigger_refetch=True,
                dependent_widgets=[widget_id],
                display_info=DisplayInfo(
                    views=["page"],
                    show_label=False,
                    row=2,
                    col=4,
                    row_span=1,
                    col_span=3,
                ),
            ),
            Parameter(
                id=5,
                name="account_id",
                label="Account",
                group_id=ParameterGroupId(0),
                type=ParameterType.SELECT,
                value=None,
                default_value=None,
                display_info=None,
            ),
        ],
    )


def to_kvp(key: str, value: str) -> NoCodeToolIn:
    return NoCodeToolIn(
        tool="to_key_value_pair",
        parameters=[
            Parameter(
                id=6,
                group_id=ParameterGroupId(0),
                name="key_from",
                type=ParameterType.STRING,
                value=key,
                display_info=None,
            ),
            Parameter(
                id=7,
                group_id=ParameterGroupId(0),
                name="value_from",
                type=ParameterType.STRING,
                value=value,
                display_info=None,
            ),
        ],
    )


def _generate_all_transactions_widget(
    session: Session,
    user: User,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(1)

    pipeline = [
        most_recent_n(widget_id),
    ]

    return NoCodeWidgetIn(
        pipeline=pipeline,
        id=widget_id,
        name="",
        description="Most recent transactions across all account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.list,
    )


def _generate_net_worth_widget(
    session: Session,
    user: User,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(2)

    pipeline = [
        NoCodeToolIn(
            tool="all_account_balances",
        )
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="Total Balance",
        description="Total balance of all accounts",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.value_with_trend,
    )


def _generate_seperator_widget(
    session: Session,
    user: User,
    widget_id: WidgetId,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
    statement: str = "",
) -> NoCodeWidgetIn:
    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=[],
        name=statement,
        description="",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.separator,
    )


def _generate_balance_widget(
    session: Session,
    user: User,
    global_parameters: dict[str, Parameter],
    widget_id: WidgetId,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    pipeline = [
        NoCodeToolIn(
            tool="account_balance",
            parameters=[get_shared_param(session, user, widget_id, global_parameters)],
        )
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="Balance",
        description="Balance of account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.value_with_trend,
    )


def _generate_throughput_widget(
    session: Session,
    user: User,
    global_parameters: dict[str, Parameter],
    widget_id: WidgetId,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
    time_unit: SelectOption = SelectOption(key="week", value="week"),
) -> NoCodeWidgetIn:
    pipeline = [
        NoCodeToolIn(
            tool="account_throughput",
            parameters=[
                get_shared_param(session, user, widget_id, global_parameters),
                Parameter(
                    id=8,
                    group_id=ParameterGroupId(0),
                    name="time_unit",
                    label="Time Unit",
                    type=ParameterType.SELECT,
                    default_value=DefaultValue(value=time_unit),
                ),
            ],
        )
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name=f"Change this {time_unit.value}",
        description="Balance of account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.value_with_trend,
    )


def _generate_plaid_badge_widget(
    session: Session,
    user: User,
    global_parameters: dict[str, Parameter],
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(4)

    pipeline = [
        NoCodeToolIn(
            tool="plaid_enabled",
            parameters=[
                get_shared_param(session, user, widget_id, global_parameters),
            ],
        )
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="Plaid Enabled",
        description="Plaid badge of account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.badge,
    )


def _generate_sync_status_widget(
    session: Session,
    user: User,
    global_parameters: dict[str, Parameter],
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(5)

    parameters = [
        get_shared_param(session, user, widget_id, global_parameters),
    ]

    pipeline = [
        NoCodeToolIn(
            tool="last_plaid_sync",
            parameters=parameters,
        )
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="Last Sync",
        description="Last sync date of account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.badge,
    )


def _generate_list_widget(
    session: Session,
    user: User,
    global_parameters: dict[str, Parameter],
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(7)
    pipeline = [
        first_n(session, user, widget_id, global_parameters),
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="",
        description="List of transactions",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.list,
    )


def _generate_balance_update_widget(
    session: Session,
    user: User,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(8)
    widget_name = "Update Account Balance"

    account_choices = make_account_choices(session, user)

    default_value = (
        DefaultValue(value=account_choices[0]) if len(account_choices) > 0 else None
    )
    parameters = [
        Parameter(
            id=24,
            name="account_id",
            group_id=ParameterGroupId(0),
            label=None,
            type=ParameterType.SELECT,
            options=account_choices,
            default_value=default_value,
            trigger_refetch=True,
            dependent_widgets=[widget_id],
            display_info=DisplayInfo(
                views=[widget_name],
                size=DisplaySize.LARGE,
                row=1,
                col=1,
                row_span=17,
                col_span=4,
            ),
        ),
        Parameter(
            id=22,
            name="balance",
            label="Balance",
            group_id=ParameterGroupId(0),
            type=ParameterType.FLOAT,
            default_value=DefaultValue(value=0.0),
            dependent_widgets=[widget_id],
            display_info=DisplayInfo(
                views=[widget_name],
                row=41,
                col=5,
                row_span=1,
                col_span=2,
            ),
        ),
        Parameter(
            id=16,
            group_id=ParameterGroupId(0),
            name="timestamp",
            label="Timestamp",
            type=ParameterType.DATETIME,
            default_value=DefaultValue(value=datetime.now(timezone.utc).timestamp()),
            dependent_widgets=[widget_id],
            display_info=DisplayInfo(
                views=[widget_name],
                row=41,
                col=5,
                row_span=1,
                col_span=2,
            ),
        ),
        Parameter(
            id=9,
            name="submit",
            label="Submit",
            group_id=ParameterGroupId(0),
            type=ParameterType.SUBMIT,
            value=False,
            default_value=DefaultValue(value=False),
            dependent_widgets=[widget_id],
            display_info=DisplayInfo(
                views=[widget_name],
                row=41,
                col=5,
                row_span=1,
                col_span=2,
            ),
        ),
    ]

    pipeline = [
        NoCodeToolIn(
            tool="update_balance",
            parameters=parameters,
        ),
    ]

    return NoCodeWidgetIn(
        pipeline=pipeline,
        id=widget_id,
        name=widget_name,
        description="Manually Update Balance",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.form,
    )


def _generate_pie_widget(
    session: Session,
    user: User,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(9)

    pipeline = [
        NoCodeToolIn(
            tool="total_amount_per_category",
        ),
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="Amount / Category",
        description="Pie chart of transactions per category",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.pie_chart,
    )


def _generate_search_widget(
    session: Session,
    user: User,
    row: int = 1,
    col: int = 1,
    row_span: int = 1,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(588)

    pipeline = [
        NoCodeToolIn(
            tool="transaction_search",
            parameters=[
                Parameter(
                    id=588,
                    name="search_string",
                    label="Search",
                    group_id=ParameterGroupId(0),
                    type=ParameterType.STRING,
                    trigger_refetch=True,
                    dependent_widgets=[widget_id],
                    display_info=DisplayInfo(
                        views=["page"],
                        row=45,
                        col=1,
                        row_span=1,
                        col_span=3,
                    ),
                ),
            ],
        ),
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="Search Results",
        description="Search Transactions",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.list,
    )


def _generate_bar_chart_widget(
    session: Session,
    user: User,
    global_parameters: dict[str, Parameter],
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetIn:
    widget_id = WidgetId(10)

    options = [
        SelectOption(key=field_name, value=" ".join(field_name.split("_")).capitalize())
        for field_name in NoCodeTransaction.model_fields
        if field_name != "id"
    ]

    agg_parameters = [
        Parameter(
            id=10,
            group_id=ParameterGroupId(0),
            name="key_from",
            label="Group By",
            type=ParameterType.SELECT,
            options=options,
            dependent_widgets=[widget_id],
            default_value=DefaultValue(
                value=SelectOption(
                    key="date_of_transaction", value="date_of_transaction"
                )
            ),
            display_info=DisplayInfo(
                views=["page"],
                row=20,
                col=1,
                row_span=2,
                col_span=5,
            ),
        ),
        Parameter(
            id=11,
            group_id=ParameterGroupId(0),
            name="values_from",
            label="Y Axis",
            type=ParameterType.MULTI_SELECT,
            options=options,
            default_value=DefaultValue(
                value=[SelectOption(key="amount", value="amount")]
            ),
        ),
    ]

    pipeline = [
        first_n(session, user, widget_id, global_parameters),
        NoCodeToolIn(
            tool="aggregate",
            parameters=agg_parameters,
        ),
    ]

    return NoCodeWidgetIn(
        id=widget_id,
        pipeline=pipeline,
        name="",
        description="Bar chart of transactions per day",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.bar_chart,
    )


def generate_account_page(session: Session, user: User) -> NoCodeCanvasCreate:
    global_parameters: dict[str, Parameter] = {}

    widgets = [
        callable(session, user)
        for callable in [
            partial(
                _generate_seperator_widget,
                row=1,
                col=1,
                row_span=1,
                col_span=12,
                widget_id=WidgetId(14),
                statement="All Accounts",
            ),
            partial(_generate_net_worth_widget, row=2, col=1, row_span=3, col_span=3),
            partial(
                _generate_all_transactions_widget, row=3, col=4, row_span=9, col_span=9
            ),
            partial(_generate_pie_widget, row=5, col=1, row_span=7, col_span=3),
            partial(
                _generate_seperator_widget,
                widget_id=WidgetId(15),
                row=13,
                col=1,
                row_span=1,
                col_span=12,
                statement="Account Specific",
            ),
            partial(
                _generate_plaid_badge_widget,
                row=14,
                col=5,
                row_span=2,
                col_span=2,
                global_parameters=global_parameters,
            ),
            partial(
                _generate_balance_update_widget, row=14, col=10, row_span=2, col_span=2
            ),
            partial(
                _generate_sync_status_widget,
                row=14,
                col=7,
                row_span=2,
                col_span=2,
                global_parameters=global_parameters,
            ),
            partial(
                _generate_balance_widget,
                row=17,
                col=1,
                row_span=3,
                col_span=4,
                global_parameters=global_parameters,
                widget_id=WidgetId(78),
            ),
            partial(
                _generate_throughput_widget,
                widget_id=WidgetId(16),
                row=17,
                col=5,
                row_span=3,
                col_span=4,
                time_unit=SelectOption(key="week", value="week"),
                global_parameters=global_parameters,
            ),
            partial(
                _generate_throughput_widget,
                widget_id=WidgetId(17),
                row=17,
                col=9,
                row_span=3,
                col_span=4,
                time_unit=SelectOption(key="month", value="month"),
                global_parameters=global_parameters,
            ),
            partial(
                _generate_bar_chart_widget,
                row=22,
                col=1,
                row_span=6,
                col_span=12,
                global_parameters=global_parameters,
            ),
            partial(
                _generate_list_widget,
                row=29,
                col=1,
                row_span=15,
                col_span=12,
                global_parameters=global_parameters,
            ),
        ]
    ]

    parameter_groups = [
        ParameterGroupOut(
            id=ParameterGroupId(0),
            type=ParameterGroupType.GLOBAL,
            name="Global Parameters",
        ),
    ]

    param_lookup: dict[int, Parameter] = {}
    for w in widgets:
        for param in extract_parameters_from_pipeline(w.pipeline, session, user):
            param_lookup[param.id] = param

    return NoCodeCanvasCreate(
        name="Account Page",
        widgets=widgets,
        parameters=list(param_lookup.values()),
        parameter_groups=parameter_groups,
    )
