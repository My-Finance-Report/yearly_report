#!/bin/bash

set -e

AWS_REGION="us-east-2"
ECR_REPO_NAME="finance-fullstack"
IMAGE_TAG="latest"
REMOTE_SERVER="finance"
REMOTE_DIR="/home/ec2-user/code/finance_app"

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

ECR_BACKEND_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/backend:${IMAGE_TAG}"
ECR_WORKER_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/worker:${IMAGE_TAG}"
ECR_FRONTEND_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/frontend:${IMAGE_TAG}"

echo "🔑 Logging into Amazon ECR..."
aws ecr get-login-password --region ${AWS_REGION} --profile ${AWS_PROFILE} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

echo "🐳 Building Backend Image..."
docker build --platform linux/amd64 -t finance-backend:${IMAGE_TAG} -f backend/Dockerfile backend/

echo "🐳 Building Worker Image..."
docker build --platform linux/amd64 -t finance-worker:${IMAGE_TAG} -f backend/Dockerfile.worker backend/

echo "🐳 Building Frontend Image..."
docker build --platform linux/amd64 --build-arg VITE_API_URL=$VITE_API_URL -t finance-frontend:${IMAGE_TAG} -f frontend/Dockerfile frontend/

echo "🏷️ Tagging Images..."
docker tag finance-backend:${IMAGE_TAG} ${ECR_BACKEND_URL}
docker tag finance-worker:${IMAGE_TAG} ${ECR_WORKER_URL}
docker tag finance-frontend:${IMAGE_TAG} ${ECR_FRONTEND_URL}

echo "🚀 Pushing Images to ECR..."
docker push ${ECR_BACKEND_URL}
docker push ${ECR_WORKER_URL}
docker push ${ECR_FRONTEND_URL}

echo "🔄 Syncing Deployment Files..."
rsync -avz docker-compose.prod.yml ${REMOTE_SERVER}:${REMOTE_DIR}/
rsync -avz bin/backup_db ${REMOTE_SERVER}:${REMOTE_DIR}/

echo "🚀 Running Deployment on ${REMOTE_SERVER}..."
ssh ${REMOTE_SERVER} << EOF
    set -e
    cd ${REMOTE_DIR}
    aws ecr get-login-password --region ${AWS_REGION} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

    echo "Pulling containers..."
    docker-compose --env-file .env.production -f docker-compose.prod.yml pull

    echo "🚀 Starting containers..."
    docker-compose --env-file .env.production -f docker-compose.prod.yml up -d

    chmod +x /home/ec2-user/code/finance_app/backup_db
EOF

echo "✅ Deployment Completed!"

