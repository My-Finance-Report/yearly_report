#! /usr/bin/env bash

set -e
set -x

cd backend
python3 -c "import app.main; import json; print(json.dumps(app.main.app.openapi()))" > ../openapi.json
cd ..
mv openapi.json frontend/
cd frontend
tail -n 1 openapi.json | tee openapi.json
npm run generate-client
