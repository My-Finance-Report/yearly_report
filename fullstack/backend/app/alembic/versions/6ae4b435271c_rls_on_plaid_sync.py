"""rls on plaid sync

Revision ID: 6ae4b435271c
Revises: 525c2b139771
Create Date: 2025-03-24 19:05:11.871138

"""
from alembic import op
import sqlalchemy as sa
import sqlmodel.sql.sqltypes


# revision identifiers, used by Alembic.
revision = '6ae4b435271c'
down_revision = '525c2b139771'
branch_labels = None
depends_on = None


def upgrade():
    # Grant permissions to app_user for plaid_sync_log table
    connection = op.get_bind()
    
    # Enable RLS on plaid_sync_log
    connection.execute(sa.text("ALTER TABLE plaid_sync_log ENABLE ROW LEVEL SECURITY;"))
    
    # Grant necessary permissions to app_user
    connection.execute(sa.text("GRANT SELECT, INSERT, UPDATE ON plaid_sync_log TO app_user;"))
    
    print("✔ RLS and permissions applied to plaid_sync_log")


def downgrade():
    # Revoke permissions and remove RLS
    connection = op.get_bind()
    
    # Revoke permissions from app_user
    connection.execute(sa.text("REVOKE SELECT, INSERT, UPDATE ON plaid_sync_log FROM app_user;"))
    
    # Remove RLS policy
    connection.execute(sa.text("DROP POLICY IF EXISTS user_isolation_policy ON plaid_sync_log;"))
    
    # Disable RLS
    connection.execute(sa.text("ALTER TABLE plaid_sync_log DISABLE ROW LEVEL SECURITY;"))
    
    print("❌ RLS and permissions removed from plaid_sync_log")