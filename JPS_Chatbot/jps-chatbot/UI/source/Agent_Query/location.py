import os
import os.path
import sys


def get_parent_path(path):
    parent, child = os.path.split(path)
    if child:
        return parent
    else:
        raise RuntimeError("Unable to determine parent for path %s" % path)


AGENT_QUERY_DIR = os.path.dirname(os.path.abspath(__file__))
AGENT_OWL_DIR = os.path.join(AGENT_QUERY_DIR, 'Agents')
JPS_DICT_DIR = os.path.join(AGENT_QUERY_DIR, 'JPS_DICTS')
ROOT_DIR = get_parent_path(get_parent_path(AGENT_QUERY_DIR))
SOURCE_DIR = os.path.join(ROOT_DIR, 'source')
DATA_PREPARATION_DIR = os.path.join(ROOT_DIR, 'data_preparation')
FILE_DIR = os.path.join(DATA_PREPARATION_DIR, 'files')



print('SOURCE_DIR', SOURCE_DIR)

# print('AGENT_QUERY_DIR', AGENT_QUERY_DIR)
# print('AGENT_OWL_DIR', AGENT_OWL_DIR)
# print('FILE_DIR', FILE_DIR)
# print('JPS_DICT_DIR', JPS_DICT_DIR)