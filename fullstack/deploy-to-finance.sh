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

# Check if we're on main branch
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" != "main" ]; then
    echo "❌ You must be on the main branch to deploy. Current branch: $CURRENT_BRANCH"
    exit 1
fi

# Fetch latest changes from remote
echo "📥 Fetching latest changes from remote..."
git fetch origin main

# Check if local main is behind remote main
LOCAL_COMMIT=$(git rev-parse HEAD)
REMOTE_COMMIT=$(git rev-parse origin/main)

if [ "$LOCAL_COMMIT" != "$REMOTE_COMMIT" ]; then
    echo "❌ Your local main branch is not up to date with remote."
    echo "Local commit: $LOCAL_COMMIT"
    echo "Remote commit: $REMOTE_COMMIT"
    echo "Please pull the latest changes before deploying."
    exit 1
fi

echo "✅ Branch check passed. Proceeding with deployment..."

# Check for uncommitted changes before running deploy checks
if ! git diff --quiet; then
    echo "❌ You have uncommitted changes. Please commit or stash them before deploying."
    git status
    exit 1
fi

# Run deploy checks and verify no changes were made
echo "🔍 Running pre-deploy checks..."
bin/check_for_deploy

# Check if deploy checks created any changes
if ! git diff --quiet; then
    echo "❌ Deploy checks created uncommitted changes. Please review and commit these changes:"
    git diff
    exit 1
fi

ECR_BACKEND_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/backend:${IMAGE_TAG}"
ECR_FRONTEND_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/frontend:${IMAGE_TAG}"

echo "🔑 Logging into Amazon ECR..."
aws ecr get-login-password --region ${AWS_REGION} --profile ${AWS_PROFILE} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

echo "🐳 Building Backend Image..."
docker build --platform linux/amd64 -t finance-backend:${IMAGE_TAG} -f backend/Dockerfile backend/



echo "🐳 Building Frontend Image..."
docker build --platform linux/amd64 --build-arg VITE_API_URL=$VITE_API_URL -t finance-frontend:${IMAGE_TAG} -f frontend/Dockerfile frontend/

echo "🏷️ Tagging Images..."
docker tag finance-backend:${IMAGE_TAG} ${ECR_BACKEND_URL}
docker tag finance-frontend:${IMAGE_TAG} ${ECR_FRONTEND_URL}

echo "🚀 Pushing Images to ECR..."
docker push ${ECR_BACKEND_URL}
docker push ${ECR_FRONTEND_URL}

echo "🔄 Syncing Deployment Files..."
rsync -avz docker-compose.prod.yml ${REMOTE_SERVER}:${REMOTE_DIR}/
rsync -avz bin/backup_db ${REMOTE_SERVER}:${REMOTE_DIR}/

echo "🚀 Running Deployment on ${REMOTE_SERVER}..."
ssh ${REMOTE_SERVER} << EOF
    set -e
    cd ${REMOTE_DIR}

    # Create log directory with proper permissions
    sudo mkdir -p /var/log/finance
    sudo chown -R ec2-user:ec2-user /var/log/finance
    sudo chmod -R 755 /var/log/finance

    aws ecr get-login-password --region ${AWS_REGION} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

    # Create a temporary env file for pulling containers
    grep -v "^AWS_PROFILE" .env.production > .env.production.runtime
    
    echo "Pulling containers..."
    docker-compose --env-file .env.production -f docker-compose.prod.yml pull

    echo "🚀 Starting containers..."
    # Use the modified env file without AWS_PROFILE for running
    docker-compose --env-file .env.production.runtime -f docker-compose.prod.yml up -d

    chmod +x /home/ec2-user/code/finance_app/backup_db
EOF

echo "✅ Deployment Completed!"
say "we smashed that shit to prod baby"
