import os
import yaml
import json

project_dir = "projects"
output_file = "dashboard/projects.json"
all_metadata = []

for folder in os.listdir(project_dir):
    folder_path = os.path.join(project_dir, folder)
    metadata_path = os.path.join(folder_path, "metadata.yml")

    if os.path.isfile(metadata_path):
        with open(metadata_path, 'r') as f:
            metadata = yaml.safe_load(f)
        
        metadata["folder"] = folder
        # Add relative file paths for all files except metadata.yml
        files = [
            f for f in os.listdir(folder_path)
            if f != "metadata.yml"
        ]
        metadata["files"] = [f"{folder}/{f}" for f in files]
        all_metadata.append(metadata)

with open(output_file, 'w') as f:
    json.dump(all_metadata, f, indent=2)