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
parent_dir = get_parent_path(this_dir)
SOURCE_DIR = parent_dir
WIKIDATA_QUERY_DIR = os.path.join(parent_dir, 'Wikidata_Query')
WIKI_MODELS_DIR = os.path.join(WIKIDATA_QUERY_DIR, 'models')

AGENT_QUERY_DIR = os.path.join(parent_dir, 'Agent_Query')
AGENT_MODELS_DIR = os.path.join(AGENT_QUERY_DIR, 'models')
AGENT_OWL_DIR = os.path.join(AGENT_QUERY_DIR, 'Agents')

JPS_QUERY_DIR = os.path.join(parent_dir, 'JPS_Query')
JPS_MODELS_DIR = os.path.join(JPS_QUERY_DIR, 'models')
print('SOURCE_DIR', SOURCE_DIR)
print('WIKI_MODEL_DIR', WIKI_MODELS_DIR)
print('AGENT_MODEL_DIR', AGENT_MODELS_DIR)
print('JPS_MODEL_DIR', JPS_MODELS_DIR)

