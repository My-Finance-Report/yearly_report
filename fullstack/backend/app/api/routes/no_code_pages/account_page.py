from app.db import Session
from app.models import (
    User,
)
from app.no_code.functions import (
    convert_to_pipeline,
    evaluate_pipeline,
    make_account_choices,
    generate_runtime_parameters,
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
        Parameter(name="n",label="Number of transactions", type=ParameterType.INT, value=10, default_value=10,is_runtime=True),
        Parameter(name="account_id",label="Account", type=ParameterType.SELECT, options=account_choices, default_value=account_choices[0], is_runtime=True),
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

def _generate_balance_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [NoCodeToolIn(tool="account_balance", parameters=[Parameter(name="account_id",label="Account", type=ParameterType.SELECT, options=make_account_choices(session, user), default_value=make_account_choices(session, user)[0], is_runtime=True)])]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.number

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="Balance",
        description="Balance of account",
        row=1,
        col=1,
        height=1,
        width=1,
        type=WidgetType.value_with_trend,
    ), generate_runtime_parameters(pipeline)

def _generate_throughput_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [NoCodeToolIn(tool="account_balance", parameters=[Parameter(name="account_id",label="Account", type=ParameterType.SELECT, options=make_account_choices(session, user), default_value=make_account_choices(session, user)[0], is_runtime=True)])]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.number

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="Change this week",
        description="Balance of account",
        row=1,
        col=2,
        height=1,
        width=1,
        type=WidgetType.value_with_trend,
    ), generate_runtime_parameters(pipeline)

def _generate_interest_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [NoCodeToolIn(tool="account_interest", parameters=[Parameter(name="account_id",label="Account", type=ParameterType.SELECT, options=make_account_choices(session, user), default_value=make_account_choices(session, user)[0], is_runtime=True)])]
    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.number

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="Interest",
        description="Interest of account",
        row=1,
        col=3,
        height=1,
        width=1,
        type=WidgetType.value_with_trend,
    ), generate_runtime_parameters(pipeline)




def _generate_plaid_badge_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [NoCodeToolIn(tool="plaid_enabled", parameters=[Parameter(name="account_id",label="Account", type=ParameterType.SELECT, options=make_account_choices(session, user), default_value=make_account_choices(session, user)[0], is_runtime=True)])]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.transactions

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="Plaid Enabled",
        description="Plaid badge of account",
        row=0,
        col=1,
        height=1,
        width=1,
        type=WidgetType.badge,
    ), generate_runtime_parameters(pipeline)


def _generate_sync_status_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [NoCodeToolIn(tool="last_plaid_sync", parameters=[Parameter(name="account_id",label="Account", type=ParameterType.SELECT, options=make_account_choices(session, user), default_value=make_account_choices(session, user)[0], is_runtime=True)])]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.transactions

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="Last Sync",
        description="Last sync date of account",
        row=0,
        col=1,
        height=1,
        width=1,
        type=WidgetType.badge,
    ), generate_runtime_parameters(pipeline)






def _generate_name_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [NoCodeToolIn(tool="account_name", parameters=[Parameter(name="account_id",label="Account", type=ParameterType.SELECT, options=make_account_choices(session, user), default_value=make_account_choices(session, user)[0], is_runtime=True)])]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.transactions

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="",
        description="Name of account",
        row=0,
        col=0,
        height=1,
        width=1,
        type=WidgetType.value,
    ), generate_runtime_parameters(pipeline)


def _generate_list_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [first_n(session, user)]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.transactions

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="",
        description="List of transactions",
        row=3,
        col=0,
        height=1,
        width=1,
        type=WidgetType.list,
    ), generate_runtime_parameters(pipeline)


def _generate_bar_chart_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    options = [SelectOption(key=field_name, value=" ".join(field_name.split("_")).capitalize()) for field_name in NoCodeTransaction.model_fields if field_name != "id"]

    pipeline = [first_n(session, user),
             NoCodeToolIn(
            tool="aggregate",
            parameters=[
                Parameter(name="key_from",label="X Axis", type=ParameterType.SELECT, options=options, default_value=SelectOption(key="date_of_transaction", value="date_of_transaction"), is_runtime=True), 
                Parameter(name="values_from",label="Y Axis", type=ParameterType.MULTI_SELECT, options=options, default_value=[SelectOption(key="amount", value="amount")]) 
            ]
    )]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.transactions

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="Transactions Over Time",
        description="Bar chart of transactions per day",
        row=2,
        col=0,
        height=300,
        width=1000,
        type=WidgetType.bar_chart,
    ), generate_runtime_parameters(pipeline)


def generate_account_page(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> NoCodeCanvas:

    widgets_and_runtime_parameters = [
        callable(session, user, runtime_parameters) for callable in [
            _generate_balance_widget,
            _generate_plaid_badge_widget,
            _generate_interest_widget,
            _generate_throughput_widget,
            _generate_name_widget,
            _generate_list_widget,
            _generate_bar_chart_widget,
            _generate_sync_status_widget
        ]
    ]
    widgets = []
    param_lookup: dict[str, Parameter] = {}

    for w, p in widgets_and_runtime_parameters:
        widgets.append(w)
        for param in p:
            param_lookup[param.name] = param

    return NoCodeCanvas(
        name="Account Page",
        widgets=widgets,
        runtime_parameters=list(param_lookup.values())
    )