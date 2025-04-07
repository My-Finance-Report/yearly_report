# Check if we're on main branch
CURRENT_BRANCH=$(git branch --show-current)
if [ "$CURRENT_BRANCH" != "main" ]; then
    echo "❌ You must be on the main branch to deploy. Current branch: $CURRENT_BRANCH"
    exit 1
fi

# Fetch latest changes from remote
echo "📥 Fetching latest changes from remote..."
git fetch origin main

# Check if local main is behind remote main
LOCAL_COMMIT=$(git rev-parse HEAD)
REMOTE_COMMIT=$(git rev-parse origin/main)

if [ "$LOCAL_COMMIT" != "$REMOTE_COMMIT" ]; then
    echo "❌ Your local main branch is not up to date with remote."
    echo "Local commit: $LOCAL_COMMIT"
    echo "Remote commit: $REMOTE_COMMIT"
    echo "Please pull the latest changes before deploying."
    exit 1
fi

# Check for uncommitted changes before running deploy checks
if ! git diff --quiet; then
    echo "❌ You have uncommitted changes. Please commit or stash them before deploying."
    git status
    exit 1
fi

# Run deploy checks and verify no changes were made


# Check if deploy checks created any changes
if ! git diff --quiet; then
    echo "❌ Deploy checks created uncommitted changes. Please review and commit these changes:"
    git diff
    exit 1
fi



