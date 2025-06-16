# SVA Code Library

This repository hosts the code library.

# Add content
1. Add a new folder in /projects/
2. Include code files and a metadata.yml file following the provided template

# Update the site
3. Update the project library by running python generate_projects_json.py
4. Push changes

After pushing, GitHub Pages automatically rebuilds the site and publishes dashboard updates. 
The GitHub Action currently only validates metadata.yml files, it does not remove the need for local running of generate_projects_json.py 
