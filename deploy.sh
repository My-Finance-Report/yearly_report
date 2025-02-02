#!/bin/bash

AWS_REGION="us-east-2"
ECR_REPO_NAME="finance"
IMAGE_TAG="latest"
REMOTE_SERVER="finance-bigger"
REMOTE_DIR="/home/ec2-user/year_report_finances"

# Load environment variables from .env if available
if [ -f .env ]; then
    export $(grep -v '^#' .env | xargs)
fi

if [[ -z "$AWS_ACCOUNT_ID" || -z "$AWS_PROFILE" ]]; then
    echo "Error: Missing AWS_ACCOUNT_ID or AWS_PROFILE in .env file"
    exit 1
fi

ECR_APP_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/app:${IMAGE_TAG}"
ECR_WORKER_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}/worker:${IMAGE_TAG}"

# Step 0: Minify the Tailwind CSS for the project
echo "Minifying Tailwind CSS..."
./tailwindcss -i static/css/input.css -o static/css/output.css --minify

# Step 1: Authenticate Docker to ECR
echo "Logging into Amazon ECR with profile ${AWS_PROFILE}..."
aws ecr get-login-password --region ${AWS_REGION} --profile ${AWS_PROFILE} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

# Step 2: Build Docker Images
echo "Building Docker images..."
docker build --platform linux/amd64 -t finance-app:${IMAGE_TAG} -f Dockerfile .
docker build --platform linux/amd64 -t finance-worker:${IMAGE_TAG} -f Dockerfile.worker .

if [ $? -ne 0 ]; then
  echo "❌ Failed to build Docker images"
  exit 1
fi

# Step 3: Tag Docker Images
echo "Tagging Docker images..."
docker tag finance-app:${IMAGE_TAG} ${ECR_APP_URL}
docker tag finance-worker:${IMAGE_TAG} ${ECR_WORKER_URL}

if [ $? -ne 0 ]; then
  echo "❌ Failed to tag Docker images"
  exit 1
fi

# Step 4: Push Docker Images to ECR
echo "Pushing Docker images to Amazon ECR..."
docker push ${ECR_APP_URL}
docker push ${ECR_WORKER_URL}

if [ $? -ne 0 ]; then
  echo "❌ Failed to push Docker images to ECR"
  exit 1
fi

# Step 5: Sync deployment files to remote server
echo "🔄 Syncing server/docker-compose.yml and server/deploy_server.sh to ${REMOTE_SERVER}..."
rsync -avz server/docker-compose.yml server/deploy_server.sh ${REMOTE_SERVER}:${REMOTE_DIR}/



if [ $? -ne 0 ]; then
  echo "❌ Failed to sync deployment files to ${REMOTE_SERVER}"
  exit 1
fi

# Step 6: Deploy on Remote Server
echo "🚀 Running deployment script on ${REMOTE_SERVER}..."
ssh ${REMOTE_SERVER} "cd ${REMOTE_DIR} && chmod +x deploy_server.sh && ./deploy_server.sh"

if [ $? -ne 0 ]; then
  echo "❌ Failed to deploy on ${REMOTE_SERVER}"
  exit 1
fi

echo "✅ Deployment completed successfully!"
