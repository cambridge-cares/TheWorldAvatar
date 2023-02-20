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

ROOT_DIR = get_parent_path(this_dir)
DATA_DIR = os.path.join(ROOT_DIR, 'DATA')
DATASET_DIR = os.path.join(ROOT_DIR, r'Dataset')
PUBCHEMMEDIUM_PATH = os.path.join(DATASET_DIR, r'PubChemMedium')
RESULT_DIR = os.path.join(PUBCHEMMEDIUM_PATH, r'results/TransE')
TOKENIZER_DIR = os.path.join(ROOT_DIR, r'tokenizerFast')
print(RESULT_DIR)