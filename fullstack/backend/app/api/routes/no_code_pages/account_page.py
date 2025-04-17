from app.db import Session
from app.models import (
    User,
)
from app.no_code.functions import (
    convert_to_pipeline,
    evaluate_pipeline,
)
from app.schemas.no_code import (
    NoCodeToolIn,
    NoCodeWidgetOut,
    Parameter,
    ParameterType,
    ResultTypeEnum,
    WidgetType,
)

def first_n(n: int) -> NoCodeToolIn:
    return NoCodeToolIn(
        tool="first_n_transactions",
        parameters=[
            Parameter(name="n", type=ParameterType.INT, value=n),
            Parameter(name="account_id", type=ParameterType.INT, value=1),
        ],
    )

def to_kvp(key: str, value: str)-> NoCodeToolIn:
    return NoCodeToolIn(
            tool="to_key_value_pair",
            parameters=[
                Parameter(
                    name="key_from", type=ParameterType.STRING, value=key
                ),
                Parameter(name="value_from", type=ParameterType.STRING, value=value),
            ],
        )


def generate_account_page(session:Session, user:User)-> list[NoCodeWidgetOut]:
    list_trans = [first_n(10)]
    daily_spend = [first_n(100), NoCodeToolIn(tool="group_by", parameters=[Parameter(name="group_by", value=0, type=ParameterType.SELECT)]), to_kvp()]
    account_name = [
        NoCodeToolIn(
            tool="account_name",
            parameters=[Parameter(name="id", type=ParameterType.INT, value=1)],
        )
    ]
    account_balance = [
        NoCodeToolIn(
            tool="account_balance",
            parameters=[Parameter(name="id", type=ParameterType.INT, value=1)],
        )
    ]
    get_sum = [first_n(10), NoCodeToolIn(tool="sum_transform")]
    get_avg = [first_n(10), NoCodeToolIn(tool="average_transform")]
    get_pie = [
        first_n(10),
        to_kvp("category_name", "account")
    ]

    widgets = [
        NoCodeWidgetOut(
            result_type=ResultTypeEnum.string,
            result=evaluate_pipeline(convert_to_pipeline(account_name), session, user),
            name="Account Name",
            description="Name of the account",
            row=0,
            col=0,
            height=1,
            width=1,
            type=WidgetType.value,
        ),
        NoCodeWidgetOut(
            result_type=ResultTypeEnum.string,
            result=evaluate_pipeline(convert_to_pipeline(daily_spend), session, user),
            name="Spend per day",
            description="Accounts Daily Spend over time",
            row=0,
            col=0,
            height=1,
            width=1,
            type=WidgetType.bar_chart,
        ),
        NoCodeWidgetOut(
            result_type=ResultTypeEnum.number,
            result=evaluate_pipeline(
                convert_to_pipeline(account_balance), session, user
            ),
            name="Account Balance",
            description="Balance of the account",
            row=0,
            col=1,
            height=1,
            width=1,
            type=WidgetType.value,
        ),
        NoCodeWidgetOut(
            result_type=ResultTypeEnum.transactions,
            result=evaluate_pipeline(convert_to_pipeline(list_trans), session, user),
            name="List Transactions",
            description="List of transactions",
            row=2,
            col=0,
            height=1,
            width=1,
            type=WidgetType.list,
        ),
        NoCodeWidgetOut(
            result_type=ResultTypeEnum.number,
            result=evaluate_pipeline(convert_to_pipeline(get_sum), session, user),
            name="Total deposits this month",
            description="Total of 10 transactions",
            row=1,
            col=1,
            height=1,
            width=1,
            type=WidgetType.value,
        ),
        NoCodeWidgetOut(
            result_type=ResultTypeEnum.number,
            result=evaluate_pipeline(convert_to_pipeline(get_avg), session, user),
            name="Average of transactions",
            description="Average of 10 transactions",
            row=1,
            col=0,
            height=1,
            width=1,
            type=WidgetType.value,
        ),
        NoCodeWidgetOut(
            result_type=ResultTypeEnum.transactions,
            result=evaluate_pipeline(convert_to_pipeline(get_pie), session, user),
            name="Pie Chart",
            description="Pie chart of 10 transactions",
            row=2,
            col=0,
            height=1,
            width=1,
            type=WidgetType.pie_chart,
        ),
    ]
    return widgets