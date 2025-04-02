#!/bin/bash

set -e

REMOTE_SERVER="worker"

echo "🔧 Setting up Docker on ${REMOTE_SERVER}..."
ssh ${REMOTE_SERVER} << EOF
    set -e
    
    # Update system packages
    echo "Updating system packages..."
    sudo yum update -y
    
    # Install Docker
    echo "Installing Docker..."
    sudo yum install -y docker
    
    # Start and enable Docker service
    echo "Starting Docker service..."
    sudo systemctl start docker
    sudo systemctl enable docker
    
    # Add ec2-user to docker group to run docker without sudo
    echo "Adding user to docker group..."
    sudo usermod -aG docker ec2-user
    
    # Install Docker Compose
    echo "Installing Docker Compose..."
    DOCKER_COMPOSE_VERSION=v2.24.6
    sudo curl -L "https://github.com/docker/compose/releases/download/\${DOCKER_COMPOSE_VERSION}/docker-compose-linux-x86_64" -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose
    
    # Create AWS CLI config directory if it doesn't exist
    echo "Setting up AWS credentials directory..."
    mkdir -p ~/.aws
    
    # Verify installations
    echo "Verifying installations..."
    docker --version
    /usr/local/bin/docker-compose --version
    
    echo "✅ Docker setup completed successfully!"
EOF

echo "🔄 Docker setup on ${REMOTE_SERVER} completed!"
echo "NOTE: You may need to reconnect to the server for group changes to take effect."
echo "Run 'ssh ${REMOTE_SERVER}' and then try 'docker ps' to verify."
