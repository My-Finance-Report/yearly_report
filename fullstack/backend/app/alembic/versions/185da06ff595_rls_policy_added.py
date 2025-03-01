"""rls policy added

Revision ID: 185da06ff595
Revises: e49be03aec16
Create Date: 2025-03-01 11:21:58.339947

"""
from alembic import op
import sqlalchemy as sa
import sqlmodel.sql.sqltypes
from app.models import Base


# revision identifiers, used by Alembic.
revision = '185da06ff595'
down_revision = 'e49be03aec16'
branch_labels = None
depends_on = None


def upgrade():
    connection = op.get_bind()
    
    for table in Base.metadata.tables.values():
        # Automatically apply RLS to tables that contain a `user_id` column
        if "user_id" in table.columns:
            table_name = table.name
            connection.execute(sa.text(f"ALTER TABLE {table_name} ENABLE ROW LEVEL SECURITY;"))
            connection.execute(sa.text(f"""
                CREATE POLICY user_isolation_policy ON {table_name}
                USING (user_id = current_setting('app.current_user_id')::int);
            """))
            print(f"✔ RLS applied to {table_name}")

def downgrade():
    connection = op.get_bind()
    
    for table in Base.metadata.tables.values():
        if "user_id" in table.columns:
            table_name = table.name
            connection.execute(sa.text(f"DROP POLICY IF EXISTS user_isolation_policy ON {table_name};"))
            connection.execute(sa.text(f"ALTER TABLE {table_name} DISABLE ROW LEVEL SECURITY;"))
            print(f"❌ RLS removed from {table_name}")