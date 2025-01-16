#!/bin/bash
set -a # Automatically export all variables
source .env
set +a # Disable automatic exporting

stack build --fast --file-watch --exec "bash -c \"pkill year-report-finances; stack exec year-report-finances&\""
