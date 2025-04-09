from abc import ABC
import enum
from fastapi import APIRouter, Depends
from pydantic import BaseModel

from app.db import Session, get_current_user, get_db
from app.models import (
    User,
)
from app.no_code.generators import FirstTenTransactionGenerator, ShowList, ShowValue, SumTransformation, AverageTransformation
from app.schemas.no_code import PipelineEnd, Primitive, evaluate_pipeline, lint_pipeline

router = APIRouter(prefix="/no_code", tags=["no_code"])

class ToolType(str, enum.Enum):
    first_ten_transactions = "first_ten_transactions"
    sum = "sum"
    average = "average"
    show_value = "show_value"
    show_list = "show_list"

tool_type_map = {
    ToolType.first_ten_transactions: FirstTenTransactionGenerator,
    ToolType.sum: SumTransformation,
    ToolType.average: AverageTransformation,
    ToolType.show_value: ShowValue,
    ToolType.show_list: ShowList,
}


class NoCodeTool(BaseModel):
    name: str
    description: str
    tool: ToolType


def make_tools()->list[NoCodeTool]:
    return [
        NoCodeTool(
            name="First 10 Transactions",
            description="Get the first 10 transactions for the user",
            tool=ToolType.first_ten_transactions,
        ),
        NoCodeTool(
            name="Sum",
            description="Sum of the passed values",
            tool=ToolType.sum,
        ),
        NoCodeTool(
            name="Average",
            description="Average of the passed values",
            tool=ToolType.average,
        ),
        NoCodeTool(
            name="Show Value",
            description="Show the passed value",
            tool=ToolType.show_value,
        ),
        NoCodeTool(
            name="Show List",
            description="Show the passed list",
            tool=ToolType.show_list,
        ),
    ]


@router.get("/get_no_code_tools", response_model=list[NoCodeTool])
def get_no_code_tool(
    user: User = Depends(get_current_user),
) -> list[NoCodeTool]:
    """Create a link token for Plaid Link."""
    return make_tools()


def convert_to_tools(pipeline: list[NoCodeTool]) -> list[ABC]:
    return [tool_type_map[tool.tool]() for tool in pipeline]

@router.post("/save_no_code_tool", response_model=PipelineEnd)
def save_no_code_tool(
    pipeline: list[NoCodeTool],
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> PipelineEnd:

    # lint_pipeline(convert_to_tools(pipeline))
    return evaluate_pipeline(convert_to_tools(pipeline), session, user)


    

    

