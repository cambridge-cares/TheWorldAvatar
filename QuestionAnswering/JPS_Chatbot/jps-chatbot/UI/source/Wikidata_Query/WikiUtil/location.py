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
WIKIDATA_QUERY_DIR = root_dir
WIKI_DICT_DIR = os.path.join(WIKIDATA_QUERY_DIR, 'WIKI_DICTS')
LDA_DIR = os.path.join(top_dir, 'LDA')
print('TOP_DIR', top_dir)
print('WIKIDATA_QUERY_DIR', WIKIDATA_QUERY_DIR)
print('WIKI_DICT_DIR', WIKI_DICT_DIR)
