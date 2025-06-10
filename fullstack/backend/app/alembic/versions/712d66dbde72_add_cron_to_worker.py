"""add cron to worker

Revision ID: 712d66dbde72
Revises: d6a4cd6e4dc3
Create Date: 2025-06-10 13:38:46.763923

"""
from alembic import op
import sqlalchemy as sa
import sqlmodel.sql.sqltypes

from app.alembic.helpers import add_to_enum


# revision identifiers, used by Alembic.
revision = '712d66dbde72'
down_revision = 'd6a4cd6e4dc3'
branch_labels = None
depends_on = None


def upgrade():
    # Create cron state table
    op.create_table('cron_state',
        sa.Column('id', sa.Integer(), nullable=False),
        sa.Column('job_name', sa.String(), nullable=False),
        sa.Column('last_run', sa.DateTime(), nullable=False),
        sa.PrimaryKeyConstraint('id'),
        sa.UniqueConstraint('job_name')
    )
    
    # Add new event types
    conn = op.get_bind()
    add_to_enum(conn, 'eventtype', 'DAILY')
    add_to_enum(conn, 'eventtype', 'WEEKLY')
    add_to_enum(conn, 'eventtype', 'MONTHLY')


def downgrade():
    # Drop unique constraint
    op.drop_constraint('uq_no_code_tool_widget_user_canvas', 'no_code_tool', type_='unique')
    
    # Drop cron state table
    op.drop_table('cron_state')
    
    # Remove event types
    # Note: We can't actually remove enum values in Postgres without recreating the type
    # The values will remain but unused
