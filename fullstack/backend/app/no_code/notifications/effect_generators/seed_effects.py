from app.models.effect import EffectConditionals, EffectType
from sqlalchemy.orm.session import Session
from app.no_code.notifications.effects import Effect, EffectConfig
from app.models.user import User

template = """
    Hey there! You have {{ count }} new transaction(s) in My Financé!
    {{ transactions_table }}
    {{ alter_settings }}
"""


def new_transaction_effect(session: Session, user: User) -> Effect:
    # TODO pull from database
    return Effect(
        type=EffectType.EMAIL,
        config=EffectConfig(
            frequency_days=0,
            template=template,
            subject="New Transactions in My Financé from {{ account_name }}",
        ),
        condition=EffectConditionals.COUNT_OF_TRANSACTIONS,
        conditional_parameters={"count": 0},
    )
