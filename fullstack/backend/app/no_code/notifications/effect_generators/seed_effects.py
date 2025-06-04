from app.models.effect import ConditionalParameters, EffectConditionals, EffectType
from sqlalchemy.orm.session import Session
from app.no_code.notifications.effects import Effect, EffectConfig
from app.models.user import User

new_transaction_template = """
    Hey there! You have {{ count }} <a href="https://myfinancereport.com/transactions">new transaction(s)</a> in My Financé!
    {{ transactions_table }}

    {{ alter_settings }}
    """


def new_transaction_effect(session: Session, user: User) -> Effect:
    return Effect(
        active=True,
        editable=True,
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=0,
            template=new_transaction_template,
            subject="New Transactions in My Financé from {{ account_name }}",
        ),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters=ConditionalParameters(count=0),
    )


deactivated_account_effect_template = """
    Hey there! We failed to sync {{ account_name }} over the past week, so we are marking it as deactivated.

    {{ alter_settings }}
    """


def deactivated_account_effect(session: Session, user: User) -> Effect:
    return Effect(
        active=True,
        editable=False,
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=0,
            template=deactivated_account_effect_template,
            subject="[My Financé] {{ account_name }} marked as inactive",
        ),
        condition=EffectConditionals.UNCONDITIONAL,
        conditional_parameters=ConditionalParameters(),
    )
