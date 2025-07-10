#!/usr/bin/env bash
set -euo pipefail

# Function: install_git_hook
# Args:
#   $1 - hook name (e.g., pre-commit)
install_git_hook() {
  local hook_name="$1"
  local source_path="scripts/git-hooks/$hook_name"
  local target_path=".git/hooks/$hook_name"

  echo "Installing Git hook: $hook_name"

  # Backup existing hook if needed
  if [ -f "$target_path" ] && ! grep -q "$source_path" "$target_path"; then
    mv "$target_path" "$target_path.bak"
    echo "Backed up existing $hook_name to $hook_name.bak"
  fi

  # Create symlink and ensure it's executable
  ln -sf "../../$source_path" "$target_path"

  echo "Hook installed: $hook_name - $source_path"
}

install_git_hook "pre-commit"
