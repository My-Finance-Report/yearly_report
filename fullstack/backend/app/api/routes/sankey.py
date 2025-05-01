from collections import defaultdict
from dataclasses import dataclass

from fastapi import APIRouter, Depends

from app.db import (
    Session,
    get_current_user,
    get_db,
)

from app.local_types import (
    PossibleSankeyInput,
    PossibleSankeyLinkage,
    SankeyConfigCreatePayload,
    SankeyConfigInfo,
    SankeyData,
    SankeyLink,
    SankeyNode,
    SankeySibling,
)
from app.models.category import Category, CategoryId
from app.models.sankey import SankeyConfig, SankeyInput, SankeyLinkage
from app.models.transaction import Transaction
from app.models.transaction_source import TransactionSource, TransactionSourceId
from app.models.user import User

router = APIRouter(prefix="/sankey", tags=["sankey"])


def get_id(lookup: dict[int, SankeyNode]) -> int:
    """
    Get the next available node ID by using the current length of the node lookup.
    In the future, we might want to ensure we do not re-create the same node
    multiple times if it already exists.
    """
    return len(lookup)


@dataclass(frozen=True, kw_only=True)
class SankeyLookups:
    inputs: list[SankeyInput]
    category_lookup: dict[CategoryId, Category]
    categories_by_transaction_source: dict[TransactionSourceId, list[Category]]
    transaction_source_lookup: dict[TransactionSourceId, TransactionSource]
    linkages_by_category: dict[CategoryId, list[SankeyLinkage]]
    totals_lookup: dict[CategoryId, float]


def make_lookups_for_sankey(
    session: Session, config: SankeyConfig, user: User
) -> SankeyLookups:
    inputs = session.query(SankeyInput).filter(SankeyInput.config_id == config.id).all()

    all_linkages = (
        session.query(SankeyLinkage).filter(SankeyLinkage.config_id == config.id).all()
    )

    linkages_by_category: dict[CategoryId, list[SankeyLinkage]] = defaultdict(list)
    for row in all_linkages:
        linkages_by_category[row.category_id].append(row)

    transaction_sources = (
        session.query(TransactionSource)
        .filter(TransactionSource.user_id == user.id)
        .all()
    )
    transaction_source_lookup = {row.id: row for row in transaction_sources}

    categories = session.query(Category).filter(Category.user_id == user.id).all()
    category_lookup = {}
    categories_by_transaction_source = defaultdict(list)
    for cat in categories:
        category_lookup[cat.id] = cat
        categories_by_transaction_source[cat.source_id].append(cat)

    totals_lookup: dict[CategoryId, float] = defaultdict(float)
    transactions = (
        session.query(Transaction).filter(Transaction.user_id == user.id).all()
    )
    for transaction in transactions:
        totals_lookup[transaction.category_id] = (
            totals_lookup[transaction.category_id] + transaction.amount
        )

    return SankeyLookups(
        inputs=inputs,
        category_lookup=category_lookup,
        categories_by_transaction_source=categories_by_transaction_source,
        transaction_source_lookup=transaction_source_lookup,
        linkages_by_category=linkages_by_category,
        totals_lookup=dict(totals_lookup),
    )


def get_or_create_sankey_config(session: Session, user: User) -> SankeyConfig:
    config = session.query(SankeyConfig).filter(SankeyConfig.user_id == user.id).first()
    if not config:
        config = SankeyConfig(user_id=user.id, name="Default")
        session.add(config)
        session.commit()
    return config


@router.get("/", response_model=SankeyData)
def get_sankey_data(
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> SankeyData:
    config = get_or_create_sankey_config(session, user)

    lookups = make_lookups_for_sankey(session, config, user)

    node_lookup: dict[int, SankeyNode] = {}

    links: list[SankeyLink] = []

    def create_node(name: str) -> SankeyNode:
        node_id = get_id(node_lookup)
        node = SankeyNode(id=node_id, name=name)
        node_lookup[node_id] = node
        return node

    for sankey_input in lookups.inputs:
        category = lookups.category_lookup[sankey_input.category_id]

        base_node = create_node(category.name)
        source_node = create_node(
            lookups.transaction_source_lookup[category.source_id].name
        )

        value = lookups.totals_lookup.get(category.id)

        if value:
            links.append(
                SankeyLink(source=base_node.id, target=source_node.id, value=value)
            )

        siblings = [
            cat
            for cat in lookups.categories_by_transaction_source[category.source_id]
            if cat.id != category.id
        ]

        for sibling_cat in siblings:
            sibling_node = create_node(sibling_cat.name)
            value = lookups.totals_lookup.get(sibling_cat.id)
            if value:
                links.append(
                    SankeyLink(
                        source=source_node.id, target=sibling_node.id, value=value
                    )
                )

            sibling_linkages = lookups.linkages_by_category[sibling_cat.id]

            for linkage in sibling_linkages:
                target_source = lookups.transaction_source_lookup[
                    linkage.target_source_id
                ]

                target_sibling_cats = lookups.categories_by_transaction_source[
                    target_source.id
                ]
                for cat_in_target_source in target_sibling_cats:
                    target_sibling_node = create_node(cat_in_target_source.name)
                    value = lookups.totals_lookup.get(cat_in_target_source.id)
                    if value:
                        links.append(
                            SankeyLink(
                                source=sibling_node.id,
                                target=target_sibling_node.id,
                                value=value,
                            )
                        )

    return SankeyData(nodes=list(node_lookup.values()), links=links)


@router.post("/", response_model=dict[str, bool])
def create_sankey_config(
    sankey_config: SankeyConfigCreatePayload,
    session: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, bool]:
    config = get_or_create_sankey_config(session, user)

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
    config = get_or_create_sankey_config(session, user)

    category_query = (
        session.query(
            Category.id,
            Category.name,
            Category.source_id,
            TransactionSource.name.label("source_name"),
        )
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
