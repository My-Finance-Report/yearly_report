"""rls

Revision ID: f1f7c192b807
Revises: df49ad369100
Create Date: 2025-04-11 23:23:38.710617

"""
from alembic import op
import sqlalchemy as sa
import sqlmodel.sql.sqltypes

from app.alembic.helpers import apply_and_grant_rls
from app.models.models import Base


# revision identifiers, used by Alembic.
revision = 'f1f7c192b807'
down_revision = 'df49ad369100'
branch_labels = None
depends_on = None


def upgrade():
    connection = op.get_bind()
    apply_and_grant_rls(connection)
   


def downgrade():

    connection = op.get_bind()
    
    for table in Base.metadata.tables.values():
        if "user_id" in table.columns:
            table_name = table.name
            connection.execute(sa.text(f"DROP POLICY IF EXISTS user_isolation_policy ON {table_name};"))
            connection.execute(sa.text(f"ALTER TABLE {table_name} DISABLE ROW LEVEL SECURITY;"))
            print(f"❌ RLS removed from {table_name}")
