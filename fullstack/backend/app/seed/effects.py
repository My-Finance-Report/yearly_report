from app.models.effect import (
    ConditionalParameters,
    EffectConditionals,
    EffectType,
    Effect as EffectModel,
    EventType,
)
from sqlalchemy.orm.session import Session
from app.models.user import User

new_transaction_template = """
    Hey there! You have {{ count }} <a href="https://myfinancereport.com/transactions">new transaction(s)</a> in My Financé!
    {{ transactions_table }}

    {{ alter_settings }}
    """

NEW_TRANSACTION_REF_NAME = "new_transaction"


def new_transaction_effect(session: Session, user: User) -> EffectModel:
    return EffectModel(
        user_id=user.id,
        ref_name=NEW_TRANSACTION_REF_NAME,
        name="New Transaction Notification",
        active=True,
        editable=True,
        event_type=EventType.NEW_TRANSACTION,
        effect_type=EffectType.EMAIL,
        frequency_days=0,
        template=new_transaction_template,
        subject="New Transactions in My Financé from {{ account_name }}",
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters=ConditionalParameters(count=0),
    )


deactivated_account_effect_template = """
    Hey there! We failed to sync {{ account_name }} over the past week, so we are marking it as deactivated.

    {{ alter_settings }}
    """

ACCOUNT_DEACTIVATED_REF_NAME = "account_deactivated"


def deactivated_account_effect(session: Session, user: User) -> EffectModel:
    return EffectModel(
        user_id=user.id,
        ref_name=ACCOUNT_DEACTIVATED_REF_NAME,
        name="Account Deactivated Notification",
        active=True,
        editable=True,
        event_type=EventType.ACCOUNT_DEACTIVATED,
        effect_type=EffectType.EMAIL,
        frequency_days=0,
        template=deactivated_account_effect_template,
        subject="[My Financé] {{ account_name }} marked as inactive",
        condition=EffectConditionals.UNCONDITIONAL,
        conditional_parameters=ConditionalParameters(),
    )


def make_all_effects(session: Session, user: User) -> list[EffectModel]:
    return [
        new_transaction_effect(session, user),
        deactivated_account_effect(session, user),
    ]


def seed_effects(session: Session, user: User) -> None:
    session.add_all(make_all_effects(session, user))
    session.commit()


def delete_effects(session: Session, user: User) -> None:
    session.query(EffectModel).filter(
        EffectModel.user_id == user.id,
        EffectModel.ref_name.in_(
            [NEW_TRANSACTION_REF_NAME, ACCOUNT_DEACTIVATED_REF_NAME]
        ),
    ).delete()
    session.commit()


def seed_effects_and_dont_update_existing(session: Session, user: User) -> None:
    existing_effects = (
        session.query(EffectModel)
        .filter(
            EffectModel.user_id == user.id,
            EffectModel.ref_name.in_(
                [NEW_TRANSACTION_REF_NAME, ACCOUNT_DEACTIVATED_REF_NAME]
            ),
        )
        .all()
    )

    existing_effect_names = {effect.ref_name for effect in existing_effects}

    new_effects = [
        effect
        for effect in make_all_effects(session, user)
        if effect.ref_name not in existing_effect_names
    ]

    session.add_all(new_effects)
    session.commit()


def seed_effects_with_hard_delete_of_existing(session: Session, user: User) -> None:
    delete_effects(session, user)
    seed_effects(session, user)
