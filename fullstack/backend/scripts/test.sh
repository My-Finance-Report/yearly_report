#!/usr/bin/env bash

set -e
set -x

# Change to the backend root directory
cd "$(dirname "$0")/.."

# Run tests with coverage
coverage run --source=app -m pytest
coverage report --show-missing
coverage html --title "${@-coverage}"
