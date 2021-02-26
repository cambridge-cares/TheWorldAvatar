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
WIKI_MODELS_DIR =  os.path.join(this_dir, 'models')
 
print('WIKI_MODELS_DIR', WIKI_MODELS_DIR)
