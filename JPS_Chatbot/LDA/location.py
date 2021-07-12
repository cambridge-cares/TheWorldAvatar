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
root_dir = get_parent_path(this_dir)
top_dir = get_parent_path(get_parent_path(root_dir))
LDA_DIR = os.path.join(root_dir, 'LDA')
print('LDA_DIR', LDA_DIR)