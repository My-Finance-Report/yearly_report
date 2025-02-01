#!/bin/bash

AWS_REGION="us-east-2"
ECR_REPO_NAME="finance"
IMAGE_TAG="latest"

if [ -f .env ]; then
    export $(grep -v '^#' .env | xargs)
fi

if [[ -z "$AWS_ACCOUNT_ID" || -z "$AWS_PROFILE" ]]; then
    echo "Error: Missing AWS_ACCOUNT_ID or AWS_PROFILE in .env file"
    exit 1
fi

ECR_APP_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}-app:${IMAGE_TAG}"
ECR_WORKER_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}-worker:${IMAGE_TAG}"

# Step 0: Minify the Tailwind CSS for the project
./tailwindcss -i static/css/input.css -o static/css/output.css --minify

# Step 1: Authenticate Docker to ECR
echo "Logging into Amazon ECR with profile ${AWS_PROFILE}..."
aws ecr get-login-password --region ${AWS_REGION} --profile ${AWS_PROFILE} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

# Step 2: Build Docker Images
echo "Building Docker images..."
docker build --platform linux/amd64 -t finance-app:${IMAGE_TAG} -f Dockerfile .
docker build --platform linux/amd64 -t finance-worker:${IMAGE_TAG} -f Dockerfile.worker .

if [ $? -ne 0 ]; then
  echo "Failed to build Docker images"
  exit 1
fi

# Step 3: Tag Docker Images
echo "Tagging Docker images..."
docker tag finance-app:${IMAGE_TAG} ${ECR_APP_URL}
docker tag finance-worker:${IMAGE_TAG} ${ECR_WORKER_URL}

if [ $? -ne 0 ]; then
  echo "Failed to tag Docker images"
  exit 1
fi

# Step 4: Push Docker Images to ECR
echo "Pushing Docker images to Amazon ECR..."
docker push ${ECR_APP_URL}
docker push ${ECR_WORKER_URL}

if [ $? -ne 0 ]; then
  echo "Failed to push Docker images to ECR"
  exit 1
fi

echo "Deployment script completed successfully."
