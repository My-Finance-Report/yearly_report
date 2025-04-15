from dataclasses import is_dataclass
from fastapi import APIRouter, Depends
from fastapi.exceptions import HTTPException
from sqlalchemy import Result, desc

from app.db import Session, get_current_user, get_db
from app.models import (
    User,
)
from app.no_code.functions import (
    NoCodeTool,
    NoCodeToolIn,
    Pipeline,
    ToolType,
    convert_to_pipeline,
    evaluate_pipeline,
    make_tools,
)
from app.schemas.no_code import  NoCodeWidgetIn, NoCodeWidgetOut, Parameter, ParameterType, PipelineEnd, ResultType, ResultTypeEnum, WidgetType

router = APIRouter(prefix="/no_code", tags=["no_code"])


@router.get("/get_no_code_tools", response_model=list[NoCodeTool])
def get_no_code_tool(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[NoCodeTool]:
    return make_tools(session, user)

def determine_result_type(result: ResultType)-> ResultTypeEnum:
    if is_dataclass(result):
        return ResultTypeEnum.transactions
    try:
        int(result)
        return ResultTypeEnum.number 
    except: 
        return ResultTypeEnum.string


def process_widget(session:Session, user: User,widget: NoCodeWidgetIn)-> NoCodeWidgetOut:

    result=evaluate_pipeline(convert_to_pipeline(widget.pipeline), session, user)

    return NoCodeWidgetOut(
        name=widget.name,
        description=widget.description,
        result=result,
        result_type=determine_result_type(result),
        width=widget.width,
        height=widget.height,
        col=widget.col,
        row=widget.row,
        type=widget.type
    )


@router.post("/save_no_code_tool", response_model=list[NoCodeWidgetOut])
def save_no_code_tool(
    widgets: list[NoCodeWidgetIn],
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[NoCodeWidgetOut]:
    try:
        return [process_widget(session,user,widget) for widget in  widgets]
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))

def first_n(n: int)->NoCodeToolIn:
    return NoCodeToolIn(tool=ToolType.first_ten_transactions, parameters=[Parameter(name="n", type=ParameterType.INT, value=n), Parameter(name="account_id", type=ParameterType.INT, value=1)])



@router.get("/get_no_code_dashboard", response_model=list[NoCodeWidgetOut])
def get_no_code_dashboard(
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> list[NoCodeWidgetOut]:

    list_trans = [first_n(10)]
    account_name = [NoCodeToolIn(tool=ToolType.account_name, parameters=[Parameter(name="id", type=ParameterType.INT, value=1)])]
    account_balance = [NoCodeToolIn(tool=ToolType.account_balance, parameters=[Parameter(name="id", type=ParameterType.INT, value=1)])]
    get_sum = [first_n(10), NoCodeToolIn(tool=ToolType.sum)]
    get_avg = [first_n(10), NoCodeToolIn(tool=ToolType.average)]
    get_pie = [first_n(10), NoCodeToolIn(tool=ToolType.to_key_value_pair, parameters=[Parameter(name="key_from", type=ParameterType.STRING, value="category_name"), Parameter(name="value_from", type=ParameterType.STRING, value="amount")])]

    widgets = [
        NoCodeWidgetOut(result_type=ResultTypeEnum.string,result = evaluate_pipeline(convert_to_pipeline(account_name), session, user),name="Account Name", description="Name of the account",  row=0, col=0, height=1, width=1, type=WidgetType.value),
        NoCodeWidgetOut(result_type=ResultTypeEnum.number,result = evaluate_pipeline(convert_to_pipeline(account_balance), session, user),name="Account Balance", description="Balance of the account",  row=0, col=1, height=1, width=1, type=WidgetType.value),
        NoCodeWidgetOut(result_type=ResultTypeEnum.transactions,result = evaluate_pipeline(convert_to_pipeline(list_trans), session, user),name="List Transactions", description="List of transactions", row=2, col=0, height=1, width=1, type=WidgetType.list),
        NoCodeWidgetOut(result_type=ResultTypeEnum.number,result = evaluate_pipeline(convert_to_pipeline(get_sum), session, user),name="Total of transactions", description="Total of 10 transactions",  row=1, col=1, height=1, width=1, type=WidgetType.value),
        NoCodeWidgetOut(result_type=ResultTypeEnum.number,result = evaluate_pipeline(convert_to_pipeline(get_avg), session, user),name="Average of transactions", description="Average of 10 transactions", row=1, col=0, height=1, width=1, type=WidgetType.value),
        NoCodeWidgetOut(result_type=ResultTypeEnum.transactions,result = evaluate_pipeline(convert_to_pipeline(get_pie), session, user),name="Pie Chart", description="Pie chart of 10 transactions",  row=3, col=0, height=1, width=1, type=WidgetType.pie_chart)
    ]
    return widgets
