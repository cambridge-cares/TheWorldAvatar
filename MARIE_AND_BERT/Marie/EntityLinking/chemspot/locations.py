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
ENTITY_LINKING_DIR = get_parent_path(this_dir)
CHEMSPOT_DIR = os.path.join(ENTITY_LINKING_DIR, 'chemspot')
PY4J_JAR_PATH = os.path.join(CHEMSPOT_DIR, r'bin/py4j-0.10.9.5.jar')
CHEMSPOT_JAR_PATH = os.path.join(CHEMSPOT_DIR, r'bin/chemspot.jar')
JAVA_PATH = r'C:\Program Files\Java\jdk1.8.0_181\jre\bin\java.exe'

DICT_ZIP_PATH = os.path.join(CHEMSPOT_DIR, r'bin/dict.zip')
IDS_ZIP_PATH = os.path.join(CHEMSPOT_DIR, r'bin/ids.zip')
MULTICLASS_BIN_PATH = os.path.join(CHEMSPOT_DIR, r'bin/multiclass.bin')
DATA_PATH = os.path.join(ENTITY_LINKING_DIR, r'data')
PUBCHEM500_JSONL_PATH = os.path.join(DATA_PATH, r'pubchem/pubchem500.jsonl')
print(CHEMSPOT_DIR)
print(PY4J_JAR_PATH)