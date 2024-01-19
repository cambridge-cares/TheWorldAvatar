import os
import os.path
import sys


def get_parent_path(path):
    parent, child = os.path.split(path)
    if child:
        return parent
    else:
        raise RuntimeError("Unable to determine parent for path %s" % path)


this_dir = os.path.dirname(os.path.abspath(__file__))

# WIKI_TRAINING_MATERIAL_GENERATION_FOLDER_DIR = os.path.join(this_dir, '')

DATA_PREPARATION_DIR = get_parent_path(this_dir)
TRAINING_FILES_DIR = os.path.join(DATA_PREPARATION_DIR, 'files')

print('TRAINING_FILES_DIR', TRAINING_FILES_DIR)
