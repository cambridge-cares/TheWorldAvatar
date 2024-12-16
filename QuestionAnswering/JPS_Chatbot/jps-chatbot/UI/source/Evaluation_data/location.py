import os
import os.path
import sys


def get_parent_path(path):
    parent, child = os.path.split(path)
    if child:
        return parent
    else:
        raise RuntimeError("Unable to determine parent for path %s" % path)


EVALUATION_DATA_DIR = os.path.dirname(os.path.abspath(__file__))
AGENT_EVALUATION_DIR = os.path.join(EVALUATION_DATA_DIR, 'Agent_Evaluation')

if __name__ == '__main__':
    print('EVALUATION_DATA_DIR', EVALUATION_DATA_DIR)
    print('AGENT_EVALUATION_DIR', AGENT_EVALUATION_DIR)

