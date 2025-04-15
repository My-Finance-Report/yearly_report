from dataclasses import dataclass
import enum 
from fastapi import APIRouter, Depends
from fastapi.exceptions import HTTPException

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
from app.schemas.no_code import Parameter, ParameterType, PipelineEnd
from app.no_code.generators import first_n_transactions

router = APIRouter(prefix="/no_code", tags=["no_code"])


@router.get("/get_no_code_tools", response_model=list[NoCodeTool])
def get_no_code_tool(
    _user: User = Depends(get_current_user),
) -> list[NoCodeTool]:
    return make_tools()


@router.post("/save_no_code_tool", response_model=PipelineEnd)
def save_no_code_tool(
    pipeline: list[NoCodeTool],
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> PipelineEnd:
    try:
        result: Pipeline = convert_to_pipeline(pipeline)
        return evaluate_pipeline(result, session, user)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


class WidgetType(str, enum.Enum):
    value = "value"
    list = "list"
    chart = "chart"


@dataclass
class NoCodeWidget:
    name: str
    description: str
    pipeline: list[NoCodeToolIn]
    result: PipelineEnd
    row: int
    col: int
    height: int
    width: int
    type: WidgetType


@router.get("/get_no_code_dashboard", response_model=list[NoCodeWidget])
def get_no_code_dashboard(
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> list[NoCodeWidget]:

    list_trans = [NoCodeToolIn(tool=ToolType.first_ten_transactions, parameters=[Parameter(name="n", type=ParameterType.INT, value=10)]), NoCodeToolIn(tool=ToolType.show_list)]
    get_sum = [NoCodeToolIn(tool=ToolType.first_ten_transactions, parameters=[Parameter(name="n", type=ParameterType.INT, value=10)]), NoCodeToolIn(tool=ToolType.sum), NoCodeToolIn(tool=ToolType.show_value)]
    get_avg = [NoCodeToolIn(tool=ToolType.first_ten_transactions, parameters=[Parameter(name="n", type=ParameterType.INT, value=10)]), NoCodeToolIn(tool=ToolType.average), NoCodeToolIn(tool=ToolType.show_value)]

    widgets = [
NoCodeWidget(result = evaluate_pipeline(convert_to_pipeline(list_trans), session, user),name="List Transactions", description="List of transactions", pipeline=list_trans, row=0, col=0, height=1, width=1, type=WidgetType.list),
NoCodeWidget(result = evaluate_pipeline(convert_to_pipeline(get_sum), session, user),name="Total of transactions", description="Total of 10 transactions", pipeline=get_sum, row=1, col=1, height=1, width=1, type=WidgetType.value),
NoCodeWidget(result = evaluate_pipeline(convert_to_pipeline(get_avg), session, user),name="Average of transactions", description="Average of 10 transactions", pipeline=get_avg, row=1, col=0, height=1, width=1, type=WidgetType.value)
    ]
    return widgets
