import os
import os.path
import subprocess
import sys


def get_parent_path(path):
    parent, child = os.path.split(path)
    if child:
        return parent
    else:
        raise RuntimeError("Unable to determine parent for path %s" % path)

def run_command(command):
    p = subprocess.Popen(command, shell=True,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT)
    return iter(p.stdout.readline, b'')


this_dir = os.path.dirname(os.path.abspath(__file__))

PARENT_DIR = get_parent_path(this_dir)
ROOT_DIR = get_parent_path(PARENT_DIR)
DATA_DIR = os.path.join(ROOT_DIR, 'DATA')
ARCHIVE_DIR = os.path.join(ROOT_DIR, 'archived')
TRAINING_DIR = os.path.join(DATA_DIR, 'Training')

ENTITY_LINKING_DATA_DIR = os.path.join(DATA_DIR, 'EntityLinking')
ENTITY_LINKING_CONF_DIR = os.path.join(PARENT_DIR, 'EntityLinking')

# Everything about chemspot
# your JAVA_PATH
if sys.platform == 'win32':
    JAVA_DIR = os.path.abspath(list(run_command("echo %JAVA_HOME%"))[0].decode('utf-8').strip())
    JAVA_PATH = os.path.join(JAVA_DIR, r'jre/bin/java.exe')
else:
    JAVA_DIR = os.path.abspath(list(run_command("echo $JAVA_HOME"))[0].decode('utf-8').strip())
    JAVA_PATH = os.path.join(JAVA_DIR, r'bin/java')


DEPLOYMENT_DIR = os.path.join(DATA_DIR, 'Deployment')
DICTIONARY_DIR = os.path.join(DATA_DIR, 'Dictionaries')
EVALUATION_DIR = os.path.join(DATA_DIR, 'Evaluation')



PY4J_JAR_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/py4j-0.10.9.5.jar')
CHEMSPOT_JAR_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/chemspot.jar')
DICT_ZIP_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/dict.zip')
IDS_ZIP_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/ids.zip')
MULTICLASS_BIN_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'bin/multiclass.bin')
PUBCHEM500_JSONL_PATH = os.path.join(ENTITY_LINKING_DATA_DIR, r'pubchem5000_trim3781.jsonl')

# print('============= LOCATION CHECKLIST ===============')
# print("JAVA_PATH", JAVA_PATH)
# print("JAVA_DIR", JAVA_DIR)
PRETRAINED_DIR = os.path.join(DATA_DIR, r'bert_pretrained')

print(PARENT_DIR)