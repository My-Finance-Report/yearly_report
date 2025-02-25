from decimal import Decimal
from fastapi import APIRouter, Depends, File, HTTPException, UploadFile

from app.api.deps import (
    get_current_user,
)

from app.db import Session, get_db
from ...local_types import (
    PossibleSankeyInput,
    PossibleSankeyLinkage,
    SankeyConfigCreatePayload,
    SankeyConfigInfo,
    SankeyData,
    SankeyInputCreate,
    SankeyLink,
    SankeyLinkageCreate,
    SankeyNode,
)
from ...models import (
    Category,
    SankeyConfig,
    SankeyInput,
    SankeyLinkage,
    TransactionSource,
    User,
)

router = APIRouter(prefix="/sankey", tags=["sankey"])


@router.get("/", response_model=SankeyData)
def get_sankey_data(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> SankeyData:
    config = session.query(SankeyConfig).filter(SankeyConfig.user_id == user.id).first()

    if not config:
        raise HTTPException(status_code=404, detail="No Sankey configuration found.")

    nodes: list[SankeyNode] = []
    links: list[SankeyLink] = []
    node_index_map: dict[str, int] = {}
    next_node_id = 0

    inputs = session.query(SankeyInput).filter(SankeyInput.config_id == config.id).all()
    for input_entry in inputs:
        category = (
            session.query(Category)
            .filter(Category.id == input_entry.category_id)
            .first()
        )
        if category and category.name not in node_index_map:
            node_index_map[category.name] = next_node_id
            nodes.append(SankeyNode(id=next_node_id, name=category.name))
            next_node_id += 1

    linkages = (
        session.query(SankeyLinkage).filter(SankeyLinkage.config_id == config.id).all()
    )
    # this code is awful, TODO refactor
    for linkage in linkages:
        category = (
            session.query(Category).filter(Category.id == linkage.category_id).first()
        )
        if not category:
            continue

        source = (
            session.query(TransactionSource)
            .filter(TransactionSource.id == category.source_id)
            .first()
        )

        target_source = (
            session.query(TransactionSource)
            .filter(TransactionSource.id == linkage.target_source_id)
            .first()
        )

        if not source or not target_source:
            continue

        if source.name not in node_index_map:
            node_index_map[source.name] = next_node_id
            nodes.append(SankeyNode(id=next_node_id, name=source.name))
            next_node_id += 1

        if category.name not in node_index_map:
            node_index_map[category.name] = next_node_id
            nodes.append(SankeyNode(id=next_node_id, name=category.name))
            next_node_id += 1

        if target_source.name not in node_index_map:
            node_index_map[target_source.name] = next_node_id
            nodes.append(SankeyNode(id=next_node_id, name=target_source.name))
            next_node_id += 1

        links.append(
            SankeyLink(
                source=node_index_map[source.name],
                target=node_index_map[category.name],
                value=5000,
            )
        )
        links.append(
            SankeyLink(
                source=node_index_map[category.name],
                target=node_index_map[target_source.name],
                value=5000,
            )
        )

    return SankeyData(nodes=nodes, links=links)


@router.post("/", response_model=dict[str, bool])
def create_sankey_config(
    sankey_config: SankeyConfigCreatePayload,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, bool]:
    config = (
        session.query(SankeyConfig)
        .filter(SankeyConfig.user_id == user.id)
        .one_or_none()
    )
    if not config:
        config = SankeyConfig(user_id=user.id, name="Custom Sankey Config")

    session.add(config)
    session.commit()

    for input in sankey_config.inputs:
        session.add(SankeyInput(config_id=config.id, category_id=input.category_id))

    for link in sankey_config.links:
        session.add(
            SankeyLinkage(
                config_id=config.id,
                category_id=link.category_id,
                target_source_id=link.target_source_id,
            )
        )

    session.commit()
    return {"success": True}


@router.get("/config-info", response_model=SankeyConfigInfo)
def get_sankey_config_info(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> SankeyConfigInfo:

    category_query = (
        session.query(Category.id, Category.name)
        .filter(Category.user_id == user.id)
        .all()
    )

    source_query = session.query(TransactionSource.id, TransactionSource.name).filter(
        TransactionSource.user_id == user.id
    )

    inputs = [
        PossibleSankeyInput(category_id=row.id, category_name=row.name)
        for row in category_query
    ]

    linkages = []
    for source in source_query:

        for category in category_query:
            linkages.append(
                PossibleSankeyLinkage(
                    category_id=category.id,
                    category_name=category.name,
                    target_source_id=source.id,
                    target_source_name=source.name,
                )
            )

    return SankeyConfigInfo(possible_inputs=inputs, possible_links=linkages)
