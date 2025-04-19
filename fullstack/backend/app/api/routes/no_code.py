from dataclasses import is_dataclass
import enum
from typing import Any
import uuid

from fastapi import APIRouter, Depends
from fastapi.exceptions import HTTPException
from pydantic import Json

from app.db import Session, get_current_user, get_db
from app.models import (
    User,
)
from app.no_code.decoration import make_tools
from app.no_code.functions import (
    convert_to_callable_pipeline,
    determine_result_type,
    evaluate_pipeline,
    extract_parameters_from_pipeline,
)
from app.schemas.no_code import (
    NoCodeCanvas,
    NoCodeTool,
    NoCodeWidgetIn,
    NoCodeWidgetOut,
    Parameter,
    ParameterType,
    ResultType,
    ResultTypeEnum,
)
from app.api.routes.no_code_pages.account_page import generate_account_page

router = APIRouter(prefix="/no_code", tags=["no_code"])


@router.get("/get_no_code_tools", response_model=list[NoCodeTool])
def get_no_code_tool(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[NoCodeTool]:
    return make_tools(session, user)


def process_widget(
    session: Session, user: User, widget: NoCodeWidgetIn
) -> NoCodeWidgetOut:
    result = evaluate_pipeline(convert_to_callable_pipeline(widget.pipeline), session, user)

    return NoCodeWidgetOut(
        id=str(uuid.uuid4().hex),
        parameters=extract_parameters_from_pipeline(widget.pipeline),
        name=widget.name,
        description=widget.description,
        result=result,
        result_type=determine_result_type(result),
        col_span=widget.width,
        row_span=widget.height,
        col=widget.col,
        row=widget.row,
        type=widget.type,
    )


@router.post("/save_no_code_tool", response_model=list[NoCodeWidgetOut])
def save_no_code_tool(
    widgets: list[NoCodeWidgetIn],
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> list[NoCodeWidgetOut]:
    try:
        return [process_widget(session, user, widget) for widget in widgets]

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


class PageVariant(str, enum.Enum):
    accounts = "accounts"


@router.get("/parameter", response_model=Parameter)
def get_parameter(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> Parameter:
    return Parameter(
        name="parameter",
        label="Parameter",
        type=ParameterType.SELECT,
        value="",
        default_value="",
        options=None,
        is_runtime=True,
    )


@router.post("/get_no_code_dashboard", response_model=NoCodeCanvas)
def get_no_code_dashboard(
    variant: PageVariant,
    no_code_parameters: list[Parameter] | None = None,
    user: User = Depends(get_current_user),
    session: Session = Depends(get_db),
) -> NoCodeCanvas:
    callable = PAGES_LOOKUP[variant]
    return callable(session, user, no_code_parameters)


PAGES_LOOKUP = {PageVariant.accounts: generate_account_page}
