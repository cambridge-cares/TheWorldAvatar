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

WIKIDATA_QUERY_DIR = os.path.join(this_dir, 'Wikidata_Query')
WIKI_MODELS_DIR = os.path.join(WIKIDATA_QUERY_DIR, 'models')
AGENT_QUERY_DIR = os.path.join(this_dir, 'Agent_Query')
AGENT_MODELS_DIR = os.path.join(AGENT_QUERY_DIR, 'models')
AGENT_OWL_DIR = os.path.join(AGENT_QUERY_DIR, 'Agents')

print('WIKI_MODELS_DIR', WIKI_MODELS_DIR)
print('AGENT_MODELS_DIR', AGENT_MODELS_DIR)