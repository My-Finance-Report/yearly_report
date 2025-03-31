#!/bin/bash

set -e

AWS_REGION="us-east-2"
ECR_REPO_NAME="finance-fullstack"
IMAGE_TAG="latest"
REMOTE_SERVER="worker"
REMOTE_DIR="/home/ec2-user/code/finance_worker"

# Load environment variables from .env.production
if [ -f .env.production ]; then
    export $(grep -v '^#' .env.production | xargs)
fi

if [[ -z "$AWS_ACCOUNT_ID" || -z "$AWS_PROFILE" ]]; then
    echo "❌ Missing AWS_ACCOUNT_ID or AWS_PROFILE in .env.production"
    exit 1
fi

# do some pre-flight checks
bin/check_for_deploy

ECR_WORKER_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/worker:${IMAGE_TAG}"

echo "🔑 Logging into Amazon ECR..."
aws ecr get-login-password --region ${AWS_REGION} --profile ${AWS_PROFILE} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

echo "🐳 Building Worker Image..."
docker build --platform linux/amd64 -t finance-worker:${IMAGE_TAG} -f backend/Dockerfile.worker backend/

echo "🏷️ Tagging Worker Image..."
docker tag finance-worker:${IMAGE_TAG} ${ECR_WORKER_URL}

echo "🚀 Pushing Worker Image to ECR..."
docker push ${ECR_WORKER_URL}

echo "🔄 Syncing Deployment Files..."
rsync -avz docker-compose.worker.yml ${REMOTE_SERVER}:${REMOTE_DIR}/docker-compose.yml

echo "🚀 Running Deployment on ${REMOTE_SERVER}..."
ssh ${REMOTE_SERVER} << EOF
    set -e
    cd ${REMOTE_DIR}
    
    # Login to ECR
    echo "Logging into ECR on remote server..."
    aws ecr get-login-password --region ${AWS_REGION} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

    # Create a runtime env file without AWS_PROFILE
    echo "Creating runtime environment file..."
    grep -v "^AWS_PROFILE" .env.production > .env.production.runtime
    
    # Pull the latest worker image
    echo "Pulling worker container..."
    docker pull ${ECR_WORKER_URL}
    
    # Stop any existing containers
    echo "Stopping existing containers..."
    docker-compose down || true
    
    # Start the worker container
    echo "🚀 Starting worker container..."
    docker-compose --env-file .env.production.runtime up -d
    
    # Show running containers
    echo "Verifying deployment..."
    docker ps
EOF

echo "✅ Worker Deployment Completed!"
