#!/bin/bash

# This script generates the HTML text for the email notification
# generated with a new version of the TWA Visualisation Framework
# (TWA-VF) is released.
#
# Note that this script also expects Python to have been installed and setup
# to run the markdown2html.py script within thie directory.
#
# Author: Michael Hillman (mdhillman<@>cmcl.io)

import os
import markdown

# Read version from file
with open('./web/twa-vis-framework/library/VERSION', 'r') as f:
    version = f.read()
print("Have read version file.")

# Read the top most section of the patch notes
notes = ""
with open('./web/twa-vis-framework/library/CHANGELOG.md', 'r') as f:
    lines = f.readlines()
    section = 0

    for line in lines:
        # Count the section
        if line.startswith('# '):
            section = section + 1
        
        # Only gather section 1
        if section == 1:
          notes = notes + "|" + line  

print("Have read change log file.")

# Convert from markdown in to HTML
notes = markdown.markdown(notes.replace("|", "\n"))
print("Have converted change log to HTML.")

# Read template email file
with open('./web/twa-vis-framework/library/resources/release.html', 'r') as f:
    template = f.read()
print("Have read template email file.")

# String substitutions
email = template.replace("[VERSION]", version)
email = email.replace("[NOTES]", notes)
print("Have made substitutions.")

# Write to file
target = os.path.expanduser('~')
target = os.path.join(target, 'email.html')

with open(target, 'w') as f:
    f.write(email)
print("Have written out final email file.")