  #!/bin/bash

# Variables (update these with your own details)
AWS_REGION="us-east-2"
AWS_ACCOUNT_ID="067448242226"
ECR_REPO_NAME="finance"
IMAGE_TAG="latest"

# Derived variables
ECR_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}:${IMAGE_TAG}"

# Step 1: Authenticate Docker to ECR
echo "Logging into Amazon ECR..."
aws ecr get-login-password --region ${AWS_REGION} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

if [ $? -ne 0 ]; then
  echo "Failed to log in to ECR"
  exit 1
fi

# Step 2: Build the Docker container
echo "Building the Docker container..."
docker build --platform linux/amd64 -t ${ECR_REPO_NAME}:${IMAGE_TAG} .

if [ $? -ne 0 ]; then
  echo "Failed to build Docker image"
  exit 1
fi

# Step 3: Tag the Docker container
echo "Tagging the Docker container..."
docker tag ${ECR_REPO_NAME}:${IMAGE_TAG} ${ECR_URL}

if [ $? -ne 0 ]; then
  echo "Failed to tag Docker image"
  exit 1
fi

# Step 4: Push the Docker container to ECR
echo "Pushing the Docker container to Amazon ECR..."
docker push ${ECR_URL}

if [ $? -ne 0 ]; then
  echo "Failed to push Docker image to ECR"
  exit 1
fi

echo "Deployment script completed successfully."
