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

PARENT_DIR = get_parent_path(this_dir)
ROOT_DIR = get_parent_path(PARENT_DIR)
DATA_DIR = os.path.join(ROOT_DIR, 'DATA')
ENTITY_LINKING_DIR = os.path.join(ROOT_DIR,'Marie', 'EntityLinking')
ENTITY_LINKING_CONF_DIR = os.path.join(ENTITY_LINKING_DIR,'conf')
ENTITY_LINKING_DATA_DIR = os.path.join(DATA_DIR,'EntityLinking')

DATASET_DIR = os.path.join(ROOT_DIR, r'Dataset/PubchemMiniFull')
EMBEDDING_DIR = os.path.join(DATASET_DIR, 'embeddings/transe')

#Everything about chemspot
#your JAVA_PATH
JAVA_PATH = r'C:\Program Files\Java\jdk1.8.0_111\jre\bin\java.exe'
PY4J_JAR_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/py4j-0.10.9.5.jar')
CHEMSPOT_JAR_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/chemspot.jar')

DICT_ZIP_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/dict.zip')
IDS_ZIP_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/ids.zip')
MULTICLASS_BIN_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/multiclass.bin')
PUBCHEM500_JSONL_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'pubchem500.jsonl')


