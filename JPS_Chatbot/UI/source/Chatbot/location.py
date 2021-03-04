import os
import os.path
import sys

def get_parent_path(path):
    parent,child = os.path.split(path)
    if child:
        return parent
    else:
        raise RuntimeError("Unable to determine parent for path %s" % path)
this_dir = os.path.dirname(os.path.abspath(__file__))
root_dir = get_parent_path(this_dir)
WIKI_DICT_DIR = os.path.join(root_dir, 'search_engine')
LDA_DIR = os.path.join(root_dir, 'Chatbot')
print('WIKI_DICT_DIR', WIKI_DICT_DIR)
