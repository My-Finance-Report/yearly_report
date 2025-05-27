
import sqlalchemy as sa
from app.models.models import Base

def apply_and_grant_rls(connection)->None:
    for table in Base.metadata.tables.values():
        if "user_id" in table.columns:
            table_name = table.name

            policy_exists = connection.execute(sa.text(f"""
                SELECT 1 FROM pg_policies 
                WHERE tablename = :table_name 
                AND policyname = 'user_isolation_policy'
            """), {'table_name': table_name}).scalar()
            if not policy_exists:
                connection.execute(sa.text(f'ALTER TABLE "{table_name}" ENABLE ROW LEVEL SECURITY;'))
                connection.execute(sa.text(f"""
                    CREATE POLICY user_isolation_policy ON "{table_name}"
                    USING (user_id = current_setting('app.current_user_id')::int);
                """))

                connection.execute(sa.text(f"""
                    GRANT INSERT, SELECT, UPDATE ON "{table_name}" TO app_user;
                """))

                print(f"✔ RLS applied to {table_name}")
            else:
                print(f"✓ Policy already exists for {table_name}")

    result = connection.execute(sa.text("""
        SELECT sequence_schema, sequence_name
        FROM information_schema.sequences
        WHERE sequence_schema NOT IN ('pg_catalog', 'information_schema')
    """))

    for schema, sequence in result:
        connection.execute(sa.text(f"""
            GRANT USAGE, UPDATE ON SEQUENCE {schema}.{sequence} TO app_user;
        """))

        print(f"✔ Granted permissions on sequence {schema}.{sequence}")
