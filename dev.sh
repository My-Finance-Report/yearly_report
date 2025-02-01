#!/bin/bash
set -a # Automatically export all variables
source .env
set +a # Disable automatic exporting

# Kill any running instances of the main app and worker
pkill -f year-report-finances
pkill -f worker-task

# Run stack build in watch mode and restart both processes on changes
stack build --fast --file-watch --exec "bash -c \"
  pkill -f year-report-finances; stack exec year-report-finances &
  pkill -f worker-task; stack exec worker-task &
\""
