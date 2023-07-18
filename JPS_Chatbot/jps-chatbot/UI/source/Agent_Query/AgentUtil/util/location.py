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
parent_dir = get_parent_path(get_parent_path(this_dir))
ROOT_DIR = get_parent_path(parent_dir)
SUPER_ROOT_DIR = get_parent_path(ROOT_DIR)
WIKIDATA_QUERY_DIR = os.path.join(ROOT_DIR, 'Wikidata_Query')
WIKI_MODELS_DIR = os.path.join(WIKIDATA_QUERY_DIR, 'models')

AGENT_QUERY_DIR = os.path.join(ROOT_DIR, 'Agent_Query')
DATA_PREPARATION_DIR = os.path.join(SUPER_ROOT_DIR, 'data_preparation')
FILE_DIR = os.path.join(DATA_PREPARATION_DIR, 'files')


AGENT_MODELS_DIR = os.path.join(AGENT_QUERY_DIR, 'models')
AGENT_OWL_DIR = os.path.join(AGENT_QUERY_DIR, 'Agents')

JPS_QUERY_DIR = os.path.join(ROOT_DIR, 'JPS_Query')
JPS_MODELS_DIR = os.path.join(JPS_QUERY_DIR, 'models')

print('ROOT_DIR', ROOT_DIR)