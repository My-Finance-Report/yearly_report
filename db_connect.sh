#!/bin/bash

# Script to connect to RDS database using credentials from AWS Secrets Manager
# Usage: ./db_connect.sh [app|worker]

set -e

# Default to app user if not specified
USER_TYPE=${1:-app}

if [ "$USER_TYPE" != "app" ] && [ "$USER_TYPE" != "worker" ]; then
  echo "Error: User type must be either 'app' or 'worker'"
  echo "Usage: ./db_connect.sh [app|worker]"
  exit 1
fi

# Set secret name based on user type
if [ "$USER_TYPE" = "app" ]; then
  SECRET_NAME="app_user"
else
  SECRET_NAME="persistent_user"
fi

# Get the secret from AWS Secrets Manager
echo "Retrieving database credentials for $SECRET_NAME..."
SECRET=$(aws secretsmanager get-secret-value --secret-id "$SECRET_NAME" --region us-east-2 --query SecretString --output text)

# Extract credentials from the secret
USERNAME=$(echo $SECRET | jq -r '.username')
PASSWORD=$(echo $SECRET | jq -r '.password')
HOST=$(echo $SECRET | jq -r '.host')
PORT=$(echo $SECRET | jq -r '.port')

# Build the connection string
CONNECTION_STRING="postgresql://$USERNAME:$PASSWORD@$HOST:$PORT/postgres?sslmode=require"

# Print connection info (without password)
echo "Connecting to database at $HOST:$PORT as $USERNAME..."

# Connect to the database
PGPASSWORD="$PASSWORD" psql -h "$HOST" -p "$PORT" -U "$USERNAME" -d postgres
