#!/bin/bash

# Check if a commit message is provided
if [ -z "$1" ]; then
  echo "Error: Commit message is required."
  echo "Usage: $0 \"Your commit message\""
  exit 1
fi

# Assign the commit message from the first argument
COMMIT_MESSAGE="$1"

# Add all changes
git add .
if [ $? -ne 0 ]; then
  echo "Error: Failed to stage changes with git add."
  exit 1
fi

# Commit with the provided message
git commit -m "$COMMIT_MESSAGE"
if [ $? -ne 0 ]; then
  echo "Error: Failed to commit changes."
  exit 1
fi

# Push the changes
git push
if [ $? -ne 0 ]; then
  echo "Error: Failed to push changes."
  exit 1
fi

echo "Changes successfully pushed!"
