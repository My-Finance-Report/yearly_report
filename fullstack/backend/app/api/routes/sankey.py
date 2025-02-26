from collections import defaultdict
from fastapi import APIRouter, Depends, HTTPException

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
    SankeyLink,
    SankeyNode,
    SankeySibling,
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

    inputs = session.query(SankeyInput).filter(SankeyInput.config_id == config.id).all()
    linkages = (
        session.query(SankeyLinkage).filter(SankeyLinkage.config_id == config.id).all()
    )

    linkages_by_category = defaultdict(list)
    for row in linkages:
        linkages_by_category[row.category_id].append(row)

    transaction_sources = (
        session.query(TransactionSource)
        .filter(TransactionSource.user_id == user.id)
        .all()
    )
    transaction_source_lookup = {row.id: row for row in transaction_sources}
    categories = session.query(Category).filter(Category.user_id == user.id).all()

    categories_by_transaction_source = defaultdict(list)
    category_lookup = {}
    for cat in categories:
        category_lookup[cat.id] = cat
        categories_by_transaction_source[cat.source_id].append(cat)

    for input in inputs:
        category = category_lookup[input.category_id]
        links_from_category = linkages_by_category.get(row.category_id, [])

        sibling_categories = [
            cat
            for cat in categories_by_transaction_source[category.source_id]
            if cat.id != category.id
        ]

        source = SankeyNode(id=0, name=category.name)
        nodes.append(source)
        for index, cat in enumerate(sibling_categories, start=1):
            target = SankeyNode(id=index, name=cat.name)
            nodes.append(target)
            links.append(SankeyLink(source=source.id, target=target.id, value=500))

    return SankeyData(nodes=nodes, links=links)


@router.post("/", response_model=dict[str, bool])
def create_sankey_config(
    sankey_config: SankeyConfigCreatePayload,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, bool]:
    """
    Fully replace the user's Sankey configuration (inputs & linkages).
    If no config exists, create it; otherwise update the existing one.
    """
    # 1. Fetch or create the user's config
    config = (
        session.query(SankeyConfig)
        .filter(SankeyConfig.user_id == user.id)
        .one_or_none()
    )
    if not config:
        config = SankeyConfig(user_id=user.id, name="Custom Sankey Config")
        session.add(config)
        session.commit()

    session.query(SankeyInput).filter(SankeyInput.config_id == config.id).delete()
    session.query(SankeyLinkage).filter(SankeyLinkage.config_id == config.id).delete()
    session.commit()

    for input_data in sankey_config.inputs:
        session.add(
            SankeyInput(config_id=config.id, category_id=input_data.category_id)
        )

    for link_data in sankey_config.links:
        session.add(
            SankeyLinkage(
                config_id=config.id,
                category_id=link_data.category_id,
                target_source_id=link_data.target_source_id,
            )
        )

    session.commit()
    return {"success": True}


@router.get("/config-info", response_model=SankeyConfigInfo)
def get_sankey_config_info(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> SankeyConfigInfo:
    """
    Returns:
      SankeyConfigInfo containing:
        - possible_inputs
        - possible_links
        - existing_inputs
        - existing_links
    """

    config = (
        session.query(SankeyConfig)
        .filter(SankeyConfig.user_id == user.id)
        .order_by(SankeyConfig.id.desc())
        .first()
    )

    if not config:
        return SankeyConfigInfo(
            possible_inputs=[],
            possible_links=[],
            existing_inputs=[],
            existing_links=[],
        )

    category_query = (
        session.query(Category.id, Category.name, Category.source_id, TransactionSource.name.label("source_name"))
        .join(TransactionSource, TransactionSource.id == Category.source_id)
        .filter(Category.user_id == user.id)
        .all()
    )
    source_query = (
        session.query(TransactionSource.id, TransactionSource.name)
        .filter(TransactionSource.user_id == user.id)
        .all()
    )

    siblings_by_source_id = defaultdict(list)
    for row in category_query:
        siblings_by_source_id[row.source_id].append(
            SankeySibling(
                category_id=row.id,
                category_name=row.name,
                source_id=row.source_id,
            )
        )

    def get_siblings(source_id: int, category_id: int) -> list[SankeySibling]:
        return [
            sib
            for sib in siblings_by_source_id.get(source_id, [])
            if sib.category_id != category_id
        ]

    possible_inputs = [
        PossibleSankeyInput(
            category_id=row.id,
            category_name=row.name,
            source_name=row.source_name,
            source_id=row.source_id,
            siblings=get_siblings(row.source_id, row.id),
        )
        for row in category_query
    ]

    possible_links = []
    for source in source_query:
        for category in category_query:
            possible_links.append(
                PossibleSankeyLinkage(
                    category_id=category.id,
                    category_name=category.name,
                    target_source_id=source.id,
                    target_source_name=source.name,
                )
            )

    existing_inputs_rows = (
        session.query(
            SankeyInput.category_id.label("category_id"),
            Category.name.label("category_name"),
            Category.source_id.label("source_id"),
            TransactionSource.name.label("source_name"),
        )
        .join(Category, Category.id == SankeyInput.category_id)
        .join(TransactionSource, TransactionSource.id == Category.source_id)
        .filter(SankeyInput.config_id == config.id)
        .all()
    )
    existing_inputs = [
        PossibleSankeyInput(
            siblings=get_siblings(row.source_id, row.category_id),
            source_id=row.source_id,
            source_name=row.source_name,
            category_id=row.category_id,
            category_name=row.category_name,
        )
        for row in existing_inputs_rows
    ]

    existing_links_rows = (
        session.query(
            SankeyLinkage.category_id.label("category_id"),
            Category.name.label("category_name"),
            SankeyLinkage.target_source_id.label("target_source_id"),
            TransactionSource.name.label("target_source_name"),
        )
        .join(Category, Category.id == SankeyLinkage.category_id)
        .join(TransactionSource, TransactionSource.id == SankeyLinkage.target_source_id)
        .filter(SankeyLinkage.config_id == config.id)
        .all()
    )
    existing_links = [
        PossibleSankeyLinkage(
            category_id=row.category_id,
            category_name=row.category_name,
            target_source_id=row.target_source_id,
            target_source_name=row.target_source_name,
        )
        for row in existing_links_rows
    ]

    return SankeyConfigInfo(
        possible_inputs=possible_inputs,
        possible_links=possible_links,
        existing_inputs=existing_inputs,
        existing_links=existing_links,
    )
