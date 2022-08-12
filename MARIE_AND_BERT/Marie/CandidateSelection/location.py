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
ROOT_DIR = get_parent_path(PARENT_DIR)
DATA_DIR = os.path.join(ROOT_DIR, 'DATA')
DATASET_DIR = os.path.join(ROOT_DIR, r'Dataset/Supermini')
MODEL_DIR = os.path.join(DATASET_DIR, r'intermediate/transr')
EMBEDDING_DIR = MODEL_DIR

SCORE_MODEL_DIR = os.path.join(this_dir, 'models')
print(SCORE_MODEL_DIR)