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
UI_DIR = get_parent_path(this_dir)
ROOT_DIR = get_parent_path(UI_DIR)

# Record some locations for use elsewhere 
JPS_SPARQL_TEMPLATE_PATH = os.path.join(ROOT_DIR, "JPS_SPARQL_template.json")
RASA_DEFAULT_MODELS_DIR = os.path.join(ROOT_DIR, "rasa_default", "models")
RASA_JPS_DIR = os.path.join(ROOT_DIR, "rasa_jps")
RASA_JPS_DATA_DIR = os.path.join(ROOT_DIR, "rasa_jps", "data")
RASA_JPS_MODELS_DIR = os.path.join(UI_DIR, "rasa_jps", "models")
SPARQL_TEMPLATE_PATH = os.path.join(ROOT_DIR, "SPARQL_template.json")

SEARCH_ENGINE_DIR = os.path.join(UI_DIR, "search_engine")
TOPIC_CLASSIFIERS_DIR = os.path.join(UI_DIR, "topic_classifier")

LOOKUP_TABS_DIR = os.path.join(UI_DIR, "rasa_jps" ,"lookup_tables")
