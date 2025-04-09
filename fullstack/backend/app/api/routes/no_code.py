from typing import  TypeVar
from fastapi import APIRouter, Depends
from fastapi.exceptions import HTTPException

from app.db import Session, get_current_user, get_db
from app.models import (
    User,
)
from app.schemas.no_code import Generator, Output, PipelineEnd, Transformation
from app.no_code.functions import NoCodeTool, make_tools, convert_to_pipeline, evaluate_pipeline

router = APIRouter(prefix="/no_code", tags=["no_code"])


@router.get("/get_no_code_tools", response_model=list[NoCodeTool])
def get_no_code_tool(
    user: User = Depends(get_current_user),
) -> list[NoCodeTool]:
    return make_tools()


@router.post("/save_no_code_tool", response_model=PipelineEnd)
def save_no_code_tool(
    pipeline: list[NoCodeTool],
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> PipelineEnd:
    T = TypeVar("T")
    try:
        result: tuple[Generator[T], list[Transformation[T,T]], Output[T]] = convert_to_pipeline(pipeline)
        return evaluate_pipeline(result[0], result[1], result[2], session, user)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


    

    

