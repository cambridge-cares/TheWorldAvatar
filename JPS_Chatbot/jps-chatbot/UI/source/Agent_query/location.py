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

print('AGENT_QUERY_DIR', AGENT_QUERY_DIR)
print('AGENT_OWL_DIR', AGENT_OWL_DIR)