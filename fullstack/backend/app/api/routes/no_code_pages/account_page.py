import uuid
from app.db import Session
from app.models import (
    User,
)
from app.no_code.functions import (
    convert_to_pipeline,
    evaluate_pipeline,
    make_account_choices,
    extract_parameters_from_pipeline,
    enrich_with_runtime,
)
from app.schemas.no_code import (
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
        global_parameters=["account_id"],
        parameters=[
            Parameter(
                name="n",
                label="Number of transactions",
                type=ParameterType.INT,
                value=10,
                default_value=10,
                is_runtime=True,
            ),
            Parameter(
                name="account_id",
                label="Account",
                type=ParameterType.SELECT,
                options=account_choices,
                default_value=account_choices[0],
                is_runtime=True,
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


def _generate_balance_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeWidgetOut:
    widget_id = '3041c6c50ca6496e931ca0f5ebeacb4b'

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

    uno = enrich_with_runtime(pipeline, runtime_parameters, widget_id)
    dos = convert_to_pipeline(uno)
    result = evaluate_pipeline(dos, session, user)
    
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.number

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=extract_parameters_from_pipeline(pipeline),
        result_type=result_type,
        result=result,
        name="Balance",
        description="Balance of account",
        row=1,
        col=1,
        height=1,
        width=1,
        type=WidgetType.value_with_trend,
    )


def _generate_throughput_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeWidgetOut:
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

    result = evaluate_pipeline(
        convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)),
        session,
        user,
    )
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.number

    return NoCodeWidgetOut(
        id= 'ffbb8a09cf7344509557451d0ee95ccf',
        parameters=extract_parameters_from_pipeline(pipeline),
        result_type=result_type,
        result=result,
        name="Change this week",
        description="Balance of account",
        row=1,
        col=2,
        height=1,
        width=1,
        type=WidgetType.value_with_trend,
    )


def _generate_interest_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
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
    result = evaluate_pipeline(
        convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)),
        session,
        user,
    )
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.number

    return NoCodeWidgetOut(
        id= 'a6f9c23d607c4cfcb40a9dbbade7fcca',
        parameters=extract_parameters_from_pipeline(pipeline),
        result_type=result_type,
        result=result,
        name="Interest",
        description="Interest of account",
        row=1,
        col=3,
        height=1,
        width=1,
        type=WidgetType.value_with_trend,
    )


def _generate_plaid_badge_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
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

    result = evaluate_pipeline(
        convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)),
        session,
        user,
    )
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.string

    return NoCodeWidgetOut(
        id='5c0d5dce034b49cbb1cffb7dc34eae8e',
        parameters=extract_parameters_from_pipeline(pipeline),
        result_type=result_type,
        result=result,
        name="Plaid Enabled",
        description="Plaid badge of account",
        row=0,
        col=1,
        height=1,
        width=1,
        type=WidgetType.badge,
    )


def _generate_sync_status_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeWidgetOut:
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

    result = evaluate_pipeline(
        convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)),
        session,
        user,
    )
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.string
    params = extract_parameters_from_pipeline(pipeline)

    return NoCodeWidgetOut(
        id= 'adfdfde527c54093a2d80bf3b4764870',
        result_type=result_type,
        result=result,
        name="Last Sync",
        description="Last sync date of account",
        row=0,
        col=1,
        height=1,
        width=1,
        type=WidgetType.badge,
        parameters=params
    )


def _generate_name_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeWidgetOut:

    widget_id='ee2cce65b2ff461484d3ac55bbaa153c'


    pipeline = [
        NoCodeToolIn(
            tool="account_name",
            parameters=[
                Parameter(
                    name="account_id",
                    label="Account",
                    type=ParameterType.SELECT,
                    options=make_account_choices(session, user),
                    default_value=make_account_choices(session, user)[0],
                    is_runtime=True,
                    widget_id=widget_id,
                )
            ],
        )
    ]

    result = evaluate_pipeline(
        convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters, widget_id)),
        session,
        user,
    )
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.string

    return NoCodeWidgetOut(
        id=widget_id,
        parameters=extract_parameters_from_pipeline(pipeline),
        result_type=result_type,
        result=result,
        name="",
        description="Name of account",
        row=0,
        col=0,
        height=1,
        width=1,
        type=WidgetType.value,
    )


def _generate_list_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeWidgetOut:
    pipeline = [first_n(session, user)]

    result = evaluate_pipeline(
        convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)),
        session,
        user,
    )
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.list_

    return NoCodeWidgetOut(
        id='b1b8f19e37064d388ee7f5061eac6123',
        parameters=extract_parameters_from_pipeline(pipeline),
        result_type=result_type,
        result=result,
        name="",
        description="List of transactions",
        row=3,
        col=0,
        height=1,
        width=1,
        type=WidgetType.list,
    )


def _generate_bar_chart_widget(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeWidgetOut:

    widget_id = 'f247e60cb3514c37aacaea50a2288372'


    options = [
        SelectOption(key=field_name, value=" ".join(field_name.split("_")).capitalize())
        for field_name in NoCodeTransaction.model_fields
        if field_name != "id"
    ]

    agg_parameters = [Parameter(
                    name="key_from",
                    label="X Axis",
                    type=ParameterType.SELECT,
                    options=options,
                    default_value=SelectOption(
                        key="date_of_transaction", value="date_of_transaction"
                    ),
                    is_runtime=True,
                    widget_id=widget_id,
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


    all_params = extract_parameters_from_pipeline(pipeline)
    result = evaluate_pipeline(
        convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)),
        session,
        user,
    )
    result_type = ResultTypeEnum.deferred if not result else ResultTypeEnum.list_

    return NoCodeWidgetOut(
        id=widget_id,
        result_type=result_type,
        result=result,
        name="Transactions Over Time",
        description="Bar chart of transactions per day",
        row=2,
        col=0,
        height=300,
        width=1000,
        type=WidgetType.bar_chart,
        parameters=all_params
    )


def generate_account_page(
    session: Session, user: User, runtime_parameters: list[Parameter] | None = None
) -> NoCodeCanvas:
    widgets_and_runtime_parameters = [
        callable(session, user, runtime_parameters)
        for callable in [
            _generate_balance_widget,
            _generate_plaid_badge_widget,
            _generate_interest_widget,
            _generate_throughput_widget,
            _generate_name_widget,
            _generate_list_widget,
            _generate_bar_chart_widget,
            _generate_sync_status_widget,
        ]
    ]
    widgets = []
    global_param_lookup: dict[str, Parameter] = {}

    for w in widgets_and_runtime_parameters:
        widgets.append(w)
        for param in w.parameters:
            if param.widget_id:
                continue
            global_param_lookup[param.name] = param

    return NoCodeCanvas(
        name="Account Page",
        widgets=widgets,
        global_parameters=list(global_param_lookup.values()),
    )
