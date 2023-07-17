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
PARENT_DIR = get_parent_path(this_dir)
TRAINING_DIR = get_parent_path(get_parent_path(this_dir))
ROOT_DIR = get_parent_path(TRAINING_DIR)

PLAYGROUND_DIR = this_dir
DATASET_DIR = os.path.join(ROOT_DIR, 'Dataset')
DATA_DIR = os.path.join(ROOT_DIR, 'DATA')