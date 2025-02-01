#!/bin/bash
set -e  # Exit on error

AWS_REGION="us-east-2"
AWS_ACCOUNT_ID="067448242226"
ECR_APP_REPO="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/finance/app"
ECR_WORKER_REPO="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/finance/worker"
IMAGE_TAG="latest"

# Step 1: Remove unused images
yes | docker image prune -f

# Step 2: Authenticate Docker to ECR
echo "Logging into Amazon ECR..."
aws ecr get-login-password --region ${AWS_REGION} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

# Step 3: Pull the latest images for both the app and the worker
echo "Pulling latest app and worker images..."
docker pull ${ECR_APP_REPO}:${IMAGE_TAG}
docker pull ${ECR_WORKER_REPO}:${IMAGE_TAG}

# Step 4: Restart services using the new images
echo "Restarting services..."
docker-compose up -d 

echo "Deployment successful!"
