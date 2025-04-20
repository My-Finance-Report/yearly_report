from functools import partial

from sqlalchemy import func
from app.db import Session
from app.models import (
    Transaction,
    User,
)
from app.no_code.functions import (
    main_render_loop,
    make_account_choices,
)
from app.schemas.no_code import (
    DisplayInfo,
    DisplaySize,
    NoCodeCanvas,
    NoCodeToolIn,
    NoCodeTransaction,
    NoCodeWidgetOut,
    Parameter,
    ParameterType,
    ResultTypeEnum,
    SelectOption,
    WidgetType,
)


def first_n(session: Session, user: User) -> NoCodeToolIn:
    account_choices = make_account_choices(session, user)

    return NoCodeToolIn(
        tool="first_n_transactions",
        parameters=[
            Parameter(
                name="n",
                label="Transactions to display",
                type=ParameterType.SELECT,
                options=[
                    SelectOption(key=str(10), value=str(10)),
                    SelectOption(key=str(20), value=str(20)),
                    SelectOption(key=str(50), value=str(50)),
                    SelectOption(key=str(100), value=str(100)),
                ],
                default_value=SelectOption(key="12", value="12"),
                is_runtime=False,
            ),
            Parameter(
                name="account_id",
                label=None,
                type=ParameterType.SELECT,
                options=account_choices,
                default_value=account_choices[0],
                is_runtime=True,
                display_info=DisplayInfo(
                    size=DisplaySize.LARGE,
                    row=24,
                    col=1,
                    row_span=1,
                    col_span=4,
                ),
            ),
            Parameter(
                name="page",
                label="",
                type=ParameterType.PAGINATION,
                options=[],
                option_generator="get_pages_per_account",
                default_value=SelectOption(key="1", value="1"),
                is_runtime=True,
                display_info=DisplayInfo(
                    row=41,
                    col=5,
                    row_span=1,
                    col_span=2,
                ),
            ),
        ],
    )


def most_recent_n(widget_id: str | None = None) -> NoCodeToolIn:
    return NoCodeToolIn(
        tool="first_n_transactions",
        parameters=[
            Parameter(
                name="n",
                label="Transactions to display",
                type=ParameterType.SELECT,
                options=[
                    SelectOption(key=str(3), value=str(3)),
                    SelectOption(key=str(5), value=str(5)),
                    SelectOption(key=str(50), value=str(50)),
                    SelectOption(key=str(100), value=str(100)),
                ],
                default_value=SelectOption(key="12", value="12"),
                widget_id=widget_id,
                is_runtime=False,
            ),
            Parameter(
                name="account_id",
                label="Account",
                type=ParameterType.SELECT,
                value=None,
                default_value=None,
                is_runtime=False,
                widget_id=widget_id,
            ),
        ],
    )


def to_kvp(key: str, value: str) -> NoCodeToolIn:
    return NoCodeToolIn(
        tool="to_key_value_pair",
        parameters=[
            Parameter(name="key_from", type=ParameterType.STRING, value=key),
            Parameter(name="value_from", type=ParameterType.STRING, value=value),
        ],
    )


def _generate_all_transactions_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    widget_id = "slkdjngkjrtngqvnnlsketjnbsr"

    pipeline = [
        most_recent_n(widget_id),
    ]

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    print("response", response)

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=response.parameters,
        result_type=response.result_type,
        result=response.result,
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
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    widget_id = "jwerlekjsngvjtrwknlvskjfn"

    pipeline = [
        NoCodeToolIn(
            tool="all_account_balances",
        )
    ]

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=response.parameters,
        result_type=response.result_type,
        result=response.result,
        name="Net Worth",
        description="Net worth of all accounts",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.value_with_trend,
    )


def _generate_seperator_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
    statement: str = "",
) -> NoCodeWidgetOut:
    widget_id = "sneflnlksnfv"
    return NoCodeWidgetOut(
        id=widget_id,
        parameters=[],
        result_type=ResultTypeEnum.string,
        result=statement,
        name="",
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
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    widget_id = "3041c6c50ca6496e931ca0f5ebeacb4b"

    pipeline = [
        NoCodeToolIn(
            tool="account_balance",
            parameters=[
                Parameter(
                    name="account_id",
                    label="Account",
                    type=ParameterType.SELECT,
                    options=make_account_choices(session, user),
                    default_value=make_account_choices(session, user)[0],
                    is_runtime=True,
                )
            ],
        )
    ]

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=response.parameters,
        result_type=response.result_type,
        result=response.result,
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
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
    time_unit: SelectOption = SelectOption(key="week", value="week"),
) -> NoCodeWidgetOut:
    pipeline = [
        NoCodeToolIn(
            tool="account_throughput",
            parameters=[
                Parameter(
                    name="account_id",
                    label="Account",
                    type=ParameterType.SELECT,
                    options=make_account_choices(session, user),
                    default_value=make_account_choices(session, user)[0],
                    is_runtime=True,
                ),
                Parameter(
                    name="time_unit",
                    label="Time Unit",
                    type=ParameterType.SELECT,
                    default_value=time_unit,
                    is_runtime=False,
                ),
            ],
        )
    ]

    widget_id = "ffbb8a09cf7344509557451d0ee95ccf"

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=response.parameters,
        result_type=response.result_type,
        result=response.result,
        name=f"Change this {time_unit.value}",
        description="Balance of account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.value_with_trend,
    )


def _generate_interest_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    pipeline = [
        NoCodeToolIn(
            tool="account_interest",
            parameters=[
                Parameter(
                    name="account_id",
                    label="Account",
                    type=ParameterType.SELECT,
                    options=make_account_choices(session, user),
                    default_value=make_account_choices(session, user)[0],
                    is_runtime=True,
                )
            ],
        )
    ]
    widget_id = "a6f9c23d607c4cfcb40a9dbbade7fcca"

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=response.parameters,
        result_type=response.result_type,
        result=response.result,
        name="Interest",
        description="Interest of account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.value_with_trend,
    )


def _generate_plaid_badge_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    pipeline = [
        NoCodeToolIn(
            tool="plaid_enabled",
            parameters=[
                Parameter(
                    name="account_id",
                    label="Account",
                    type=ParameterType.SELECT,
                    options=make_account_choices(session, user),
                    default_value=make_account_choices(session, user)[0],
                    is_runtime=True,
                )
            ],
        )
    ]

    widget_id = "5c0d5dce034b49cbb1cffb7dc34eae8e"

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=response.parameters,
        result_type=response.result_type,
        result=response.result,
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
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    widget_id = "adfdfde527c54093a2d80bf3b4764870"

    pipeline = [
        NoCodeToolIn(
            tool="last_plaid_sync",
            parameters=[
                Parameter(
                    name="account_id",
                    label="Account",
                    type=ParameterType.SELECT,
                    options=make_account_choices(session, user),
                    default_value=make_account_choices(session, user)[0],
                    is_runtime=True,
                )
            ],
        )
    ]

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        result_type=response.result_type,
        result=response.result,
        name="Last Sync",
        description="Last sync date of account",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.badge,
        parameters=response.parameters,
    )


def _generate_name_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    widget_id = "ee2cce65b2ff461484d3ac55bbaa153c"

    return NoCodeWidgetOut(
        id=widget_id,
        result_type=ResultTypeEnum.string,
        result="🚧 Heads up, this page is under construction and is liable to change! There may also be some weird / half complete features. 🚧",
        name="",
        description="",
        parameters=[],
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.value,
    )


def _generate_list_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    pipeline = [first_n(session, user)]
    widget_id = "b1b8f19e37064d388ee7f5061eac6123"

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=response.parameters,
        result_type=response.result_type,
        result=response.result,
        name="",
        description="List of transactions",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.list,
    )


def _generate_pie_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    widget_id = "selkgnwbkjnsdfnvgsebkbbb"

    pipeline = [
        NoCodeToolIn(
            tool="total_amount_per_category",
        ),
    ]
    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        result_type=response.result_type,
        result=response.result,
        name="Amount / Category",
        description="Pie chart of transactions per category",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.pie_chart,
        parameters=response.parameters,
    )


def _generate_bar_chart_widget(
    session: Session,
    user: User,
    runtime_parameters: list[Parameter] | None = None,
    row: int = 1,
    col: int = 1,
    row_span: int = 3,
    col_span: int = 3,
) -> NoCodeWidgetOut:
    widget_id = "f247e60cb3514c37aacaea50a2288372"

    options = [
        SelectOption(key=field_name, value=" ".join(field_name.split("_")).capitalize())
        for field_name in NoCodeTransaction.model_fields
        if field_name != "id"
    ]

    agg_parameters = [
        Parameter(
            name="key_from",
            label="Group By",
            type=ParameterType.SELECT,
            options=options,
            default_value=SelectOption(
                key="date_of_transaction", value="date_of_transaction"
            ),
            is_runtime=True,
            widget_id=widget_id,
            display_info=DisplayInfo(
                row=30,
                col=1,
                row_span=1,
                col_span=2,
            ),
        ),
        Parameter(
            name="values_from",
            label="Y Axis",
            type=ParameterType.MULTI_SELECT,
            options=options,
            default_value=[SelectOption(key="amount", value="amount")],
        ),
    ]

    pipeline = [
        first_n(session, user),
        NoCodeToolIn(
            tool="aggregate",
            parameters=agg_parameters,
        ),
    ]

    response = main_render_loop(pipeline, session, user, runtime_parameters, widget_id)

    return NoCodeWidgetOut(
        id=widget_id,
        result_type=response.result_type,
        result=response.result,
        name="",
        description="Bar chart of transactions per day",
        row=row,
        col=col,
        row_span=row_span,
        col_span=col_span,
        type=WidgetType.bar_chart,
        parameters=response.parameters,
    )


def generate_account_page(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeCanvas:
    widgets_and_runtime_parameters = [
        callable(session, user, runtime_parameters)
        for callable in [
            partial(
                _generate_seperator_widget,
                row=4,
                col=1,
                row_span=1,
                col_span=12,
                statement="All Accounts",
            ),
            partial(_generate_name_widget, row=1, col=3, row_span=1, col_span=8),
            partial(_generate_net_worth_widget, row=5, col=1, row_span=3, col_span=3),
            partial(
                _generate_all_transactions_widget, row=5, col=4, row_span=5, col_span=9
            ),
            partial(_generate_pie_widget, row=8, col=1, row_span=3, col_span=3),
            partial(
                _generate_seperator_widget,
                row=22,
                col=1,
                row_span=1,
                col_span=12,
                statement="Account Specific",
            ),
            partial(
                _generate_plaid_badge_widget, row=24, col=5, row_span=1, col_span=2
            ),
            partial(
                _generate_sync_status_widget, row=24, col=7, row_span=1, col_span=2
            ),
            partial(_generate_balance_widget, row=28, col=1, row_span=1, col_span=4),
            partial(
                _generate_throughput_widget,
                row=28,
                col=5,
                row_span=1,
                col_span=4,
                time_unit=SelectOption(key="week", value="week"),
            ),
            partial(
                _generate_throughput_widget,
                row=28,
                col=9,
                row_span=1,
                col_span=4,
                time_unit=SelectOption(key="month", value="month"),
            ),
            partial(_generate_bar_chart_widget, row=33, col=1, row_span=2, col_span=12),
            partial(_generate_list_widget, row=36, col=1, row_span=4, col_span=12),
        ]
    ]
    widgets = []
    param_lookup: dict[str, Parameter] = {}

    for w in widgets_and_runtime_parameters:
        widgets.append(w)
        for param in w.parameters:
            param_lookup[param.name] = param

    return NoCodeCanvas(
        name="Account Page",
        widgets=widgets,
        parameters=list(param_lookup.values()),
    )
