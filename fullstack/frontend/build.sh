#! /usr/bin/env bash
set -euo pipefail

docker build --platform linux/amd64 --build-arg VITE_API_URL=$VITE_API_URL -t finance-frontend:${IMAGE_TAG} -f Dockerfile .
