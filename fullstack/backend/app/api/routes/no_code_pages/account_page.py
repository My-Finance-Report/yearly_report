from app.db import Session
from app.models import (
    User,
)
from app.no_code.functions import (
    convert_to_pipeline,
    evaluate_pipeline,
    make_account_choices,
)
from app.schemas.no_code import (
    NoCodeCanvas,
    NoCodeToolIn,
    NoCodeWidgetOut,
    Parameter,
    ParameterType,
    ResultTypeEnum,
    WidgetType,
)


def first_n(session: Session, user: User) -> NoCodeToolIn:

    return NoCodeToolIn(
        tool="first_n_transactions",
        parameters=[
        Parameter(name="n", type=ParameterType.INT, value=10, default_value=10,is_runtime=True),
        Parameter(name="account_id", type=ParameterType.SELECT, options=make_account_choices(session, user), value=None, is_runtime=True),
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

def generate_runtime_parameters(tools: list[NoCodeToolIn]) -> list[Parameter]:
    runtime_params = []
    for tool in tools:
        if tool.parameters:
            runtime_params.extend([p for p in tool.parameters if p.is_runtime])
    return runtime_params

def enrich_with_runtime(tools: list[NoCodeToolIn], runtime_parameters: list[Parameter] | None = None) -> list[NoCodeToolIn]:
    param_value_lookup = {param.name: param.value for param in runtime_parameters} if runtime_parameters else {}
    for tool in tools:
        if tool.parameters:
            for param in tool.parameters:
                if param.is_runtime:
                    param.value = param_value_lookup.get(param.name)
    return tools


def generate_account_page(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> NoCodeCanvas:

    widgets_and_runtime_parameters = [
        callable(session, user, runtime_parameters) for callable in [
            _generate_list_widget,
            _generate_bar_chart_widget
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


def _generate_list_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [first_n(session, user)]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.transactions

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="List Transactions",
        description="List of transactions",
        row=2,
        col=0,
        height=1,
        width=1,
        type=WidgetType.list,
    ), generate_runtime_parameters(pipeline)


def _generate_bar_chart_widget(session: Session, user: User, runtime_parameters: list[Parameter] | None = None) -> tuple[NoCodeWidgetOut, list[Parameter]]:

    pipeline = [first_n(session, user),
             NoCodeToolIn(
            tool="aggregate",
            parameters=[
                Parameter(name="key_from", type=ParameterType.STRING, default_value="date_of_transaction", is_runtime=True), # should be select
                Parameter(name="values_from", type=ParameterType.LIST, value=["amount"]) # should be multiselect
            ]
    )]

    result = evaluate_pipeline(convert_to_pipeline(enrich_with_runtime(pipeline, runtime_parameters)), session, user)
    result_type =  ResultTypeEnum.deferred if not result else ResultTypeEnum.transactions

    return NoCodeWidgetOut(
        result_type=result_type,
        result=result,
        name="Transactions Over Time",
        description="Bar chart of transactions per day",
        row=3,
        col=0,
        height=1,
        width=1000,
        type=WidgetType.bar_chart,
    ), generate_runtime_parameters(pipeline)
