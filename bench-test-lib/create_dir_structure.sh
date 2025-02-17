#!/bin/bash

# Function to create directory structure
create_structure() {
    local parent_dir=$1
    local depth=$2
    local width=$3

    # Stop if depth reaches zero
    if [ "$depth" -le 0 ]; then
        return
    fi

    # Create subdirectories
    for i in $(seq 1 "$width"); do
        sub_dir="${parent_dir}/dir_$i"
        mkdir -p "$sub_dir"

        # Recursively create deeper levels
        create_structure "$sub_dir" $((depth - 1)) "$width"
    done
}

# Usage check
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <root_directory> <depth> <width>"
    exit 1
fi

# Get parameters
ROOT_DIR=$1
DEPTH=$2
WIDTH=$3

# Ensure the root directory exists
mkdir -p "$ROOT_DIR"
echo "Root directory: $ROOT_DIR"

# Start creating the directory structure
create_structure "$ROOT_DIR" "$DEPTH" "$WIDTH"

echo "Directory structure creation completed."
