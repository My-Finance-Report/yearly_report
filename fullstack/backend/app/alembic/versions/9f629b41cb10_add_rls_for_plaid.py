"""add rls for plaid

Revision ID: 9f629b41cb10
Revises: 577b285a565d
Create Date: 2025-03-21 16:44:25.395533

"""
from alembic import op
import sqlalchemy as sa
import sqlmodel.sql.sqltypes


# revision identifiers, used by Alembic.
revision = '9f629b41cb10'
down_revision = '577b285a565d'
branch_labels = None
depends_on = None

def upgrade():
    # Apply row-level security to Plaid tables
    connection = op.get_bind()
    
    # Add RLS to plaid tables
    for table_name in ["plaid_item", "plaid_account"]:
        connection.execute(sa.text(f"ALTER TABLE {table_name} ENABLE ROW LEVEL SECURITY;"))
        connection.execute(sa.text(f"""
            CREATE POLICY user_isolation_policy ON {table_name}
            USING (user_id = current_setting('app.current_user_id')::int);
        """))
        print(f"✔ RLS applied to {table_name}")


def downgrade():
    # Remove row-level security from Plaid tables
    connection = op.get_bind()
    
    # Remove RLS from plaid tables
    for table_name in ["plaid_item", "plaid_account"]:
        connection.execute(sa.text(f"DROP POLICY IF EXISTS user_isolation_policy ON {table_name};"))
        connection.execute(sa.text(f"ALTER TABLE {table_name} DISABLE ROW LEVEL SECURITY;"))
        print(f"✔ RLS removed from {table_name}")

