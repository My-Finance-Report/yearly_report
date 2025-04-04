#! /usr/bin/env bash
set -euo pipefail

docker build --platform linux/amd64 -t finance-backend:${IMAGE_TAG} -f Dockerfile .
