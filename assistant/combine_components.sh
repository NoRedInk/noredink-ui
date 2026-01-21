#!/bin/bash

# This script is meant to be run from `create_or_update_assistant.py`. It
# distributes the highest version files from specified directories into a set
# number of parts.

if [ -z "$1" ]; then
    echo "Please specify the number of parts."
    exit 1
fi

num_parts=$1
if ! [[ "$num_parts" =~ ^[0-9]+$ ]]; then
    echo "The number of parts must be a positive integer."
    exit 1
fi

output_file_base="noredinkuicomponentspart"
ignore_list=("AnimatedIcon" "AssignmentIcon" "CharacterIcon" "Logo" "MasteryIcon" "Pennant" "Sprite" "UiIcon")

part=1
dir_count=0
total_dirs=$(find ../src/Nri/Ui/ -maxdepth 1 -type d | wc -l)
interval_dirs=$((total_dirs / num_parts))

output_file="${output_file_base}${part}.md"
# ensure the output file is empty before starting
: > "$output_file"


is_in_ignore_list() {
    local folder=$1
    for ignore_folder in "${ignore_list[@]}"; do
        if [[ "$folder" == *"$ignore_folder"* ]]; then
            return 0 # true, folder is in the ignore list
        fi
    done
    return 1 # false, folder is not in the ignore list
}

# concatenate the highest version file from a folder to the output file
concatenate_highest_version() {
    local folder=$1
    # use find to list files only, then sort and pick the highest version file
    highest_version_file=$(find "$folder" -maxdepth 1 -type f | sort -V | tail -n 1)
    
    if [ ! -z "$highest_version_file" ] && [ -f "$highest_version_file" ]; then
        # ensure the file is readable before attempting to concatenate
        if [ -r "$highest_version_file" ]; then
            echo -e "# $highest_version_file\n" >> "$output_file"
            cat "$highest_version_file" >> "$output_file" || {
                echo "Failed to read file: $highest_version_file"
                return 1
            }
            echo -e "\n---\n" >> "$output_file"
        else
            echo "Cannot read file: $highest_version_file"
        fi
    fi
}


for dir in ../src/Nri/Ui/*/ ; do
    if [ -d "$dir" ] && ! is_in_ignore_list "$dir"; then
        if [ "$dir_count" -ge "$interval_dirs" ] && [ "$part" -lt "$num_parts" ]; then
            part=$((part + 1))
            output_file="${output_file_base}${part}.md"
            : > "$output_file" # clear the new output file
            interval_dirs=$((interval_dirs + total_dirs / num_parts))
        fi
        concatenate_highest_version "$dir"
        dir_count=$((dir_count + 1))
    fi
done

echo "Completed. Contents of the highest version files are distributed across ${num_parts} files."
