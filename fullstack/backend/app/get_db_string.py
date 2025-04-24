import json
import os
from urllib.parse import quote_plus

import boto3
from botocore.utils import ClientError


def is_local() -> bool:
    return os.getenv("IS_LOCAL") is not None


def get_app_user_database_url() -> str:
    if is_local():
        return os.environ["DATABASE_URL"]

    secret_name = "app_user"
    return _get_database_url(secret_name)


def get_worker_database_url() -> str:
    if is_local():
        return "postgresql://persistent_user:persistent_pass@localhost:5433/persistent_db"

    secret_name = "persistent_user"
    return _get_database_url(secret_name)


def _get_database_url(secret_name: str) -> str:
    client = boto3.client("secretsmanager", region_name="us-east-2")

    try:
        get_secret_value_response = client.get_secret_value(SecretId=secret_name)
    except ClientError as e:
        raise e

    secret = json.loads(get_secret_value_response["SecretString"])

    return (
        f"postgresql://{secret['username']}:{quote_plus(secret['password'])}"
        f"@{secret['host']}:{secret['port']}/postgres?sslmode=require"
    )
