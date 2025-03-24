import boto3
import os
from urllib.parse import quote_plus

from botocore.utils import ClientError

def is_local():
    return os.getenv("IS_LOCAL") is not None

def get_app_user_database_url():
    if is_local():
        return os.environ["DATABASE_URL"]

    secret_name = "app_user"
    return _get_database_url(secret_name)

def get_worker_database_url():
    if is_local():
        return os.environ["WORKER_DATABASE_URL"]

    secret_name = "persistent_user"
    return _get_database_url(secret_name)


def _get_database_url(secret_name: str) -> str:
    region_name = "us-east-2"
    session = boto3.session.Session()
    client = session.client(
        service_name='secretsmanager',
        region_name=region_name
    )

    try:
        get_secret_value_response = client.get_secret_value(
            SecretId=secret_name
        )
    except ClientError as e:
        raise e

    secret = get_secret_value_response['SecretString']

    return (
        f"postgresql://{secret['username']}:{quote_plus(secret['password'])}"
        f"@{secret['host']}:{secret['port']}/{secret['dbname']}?sslmode=require"
    )