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

ECR_URL="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/${ECR_REPO_NAME}:${IMAGE_TAG}"

# Step 0: Minify the Tailwind CSS for the project
./tailwindcss -i static/css/input.css -o static/css/output.css --minify

# Step 1: Authenticate Docker to ECR using the specified profile
echo "Logging into Amazon ECR with profile ${AWS_PROFILE}..."
aws ecr get-login-password --region ${AWS_REGION} --profile ${AWS_PROFILE} | docker login --username AWS --password-stdin "${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"


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
