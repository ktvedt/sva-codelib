import os
import yaml
import json

# set directories
project_dir = "projects"
output_file = "dashboard/projects.json"
all_metadata = []

# run automatic metadata update
for folder in os.listdir(project_dir):
    metadata_path = os.path.join(project_dir, folder, "metadata.yml")
    if os.path.isfile(metadata_path):
        with open(metadata_path, 'r') as f:
            metadata = yaml.safe_load(f)
            all_metadata.append(metadata)

with open(output_file, 'w') as f:
    json.dump(all_metadata, f, indent=2)