from alembic import op
import sqlalchemy as sa
from sqlalchemy.sql import table, column, text
from sqlalchemy import String, Enum

# Define the ENUM type manually
jobkind_enum = sa.Enum('full_upload', 'recategorize', name='jobkind')

# revision identifiers, used by Alembic
revision = 'a7e463792cb2'
down_revision = 'aaa0e5ffa328'
branch_labels = None
depends_on = None

def upgrade():
    # Step 1: Create ENUM type explicitly
    jobkind_enum.create(op.get_bind(), checkfirst=True)

    # Step 2: Add column with nullable=True temporarily
    op.add_column('process_file_job', sa.Column('kind', jobkind_enum, nullable=True))

    # Step 3: Update existing rows using explicit CAST to ENUM
    op.execute(text("UPDATE process_file_job SET kind = CAST('full_upload' AS jobkind)"))

    # Step 4: Alter column to be NOT NULL
    op.alter_column('process_file_job', 'kind', nullable=False)

def downgrade():
    # Remove the column first
    op.drop_column('process_file_job', 'kind')

    # Drop ENUM type explicitly (ONLY if it's no longer used)
    jobkind_enum.drop(op.get_bind(), checkfirst=True)
