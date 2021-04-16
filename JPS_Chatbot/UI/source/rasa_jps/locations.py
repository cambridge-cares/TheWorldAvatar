import os
import os.path
import sys


def get_parent_path(path):
    parent, child = os.path.split(path)
    if child:
        return parent
    else:
        raise RuntimeError("Unable to determine parent for path %s" % path)


# Extract root directory path from the path of this source file
this_dir = os.path.dirname(os.path.abspath(__file__))
UI_DIR = get_parent_path(get_parent_path(this_dir))
print('UI_DIR', UI_DIR)
ROOT_DIR = get_parent_path(UI_DIR)
print('ROOT_DIR', ROOT_DIR)

LDA_DIR = os.path.join(ROOT_DIR, 'LDA')
print('LDA_DIR', LDA_DIR)

SOURCE_DIR = os.path.join(UI_DIR, 'source')
print('SOURCE_DIR', SOURCE_DIR)

RASA_JPS_DIR = os.path.join(SOURCE_DIR, 'rasa_jps')
print('RASA_JPS_DIR', RASA_JPS_DIR)

# Record some locations for use elsewhere
JPS_SPARQL_TEMPLATE_PATH = os.path.join(UI_DIR, "JPS_SPARQL_template.json")
RASA_JPS_DATA_DIR = os.path.join(RASA_JPS_DIR, "data")
RASA_JPS_MODELS_DIR = os.path.join(RASA_JPS_DIR, "models")
SPARQL_TEMPLATE_PATH = os.path.join(ROOT_DIR, "SPARQL_template.json")

SEARCH_ENGINE_DIR = os.path.join(SOURCE_DIR, "search_engine")
TOPIC_CLASSIFIERS_DIR = os.path.join(ROOT_DIR, "LDA")

LOOKUP_TABS_DIR = os.path.join(RASA_JPS_DIR, "lookup_tables")
CONFIG_PATH = os.path.join(ROOT_DIR, "config.json")