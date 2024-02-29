import os
from chemistry_and_robots.data_model import iris
RESOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
ONTOLOGY_DIR = os.path.join(RESOURCE_DIR, 'ontology')

TBOX_PATH_DICT = {
    iris.ONTODOE: os.path.join(ONTOLOGY_DIR, 'OntoDoE.owl'),
    iris.ONTOREACTION: os.path.join(ONTOLOGY_DIR, 'OntoReaction.owl'),
    iris.ONTOGOAL: os.path.join(ONTOLOGY_DIR, 'OntoGoal.owl'),
    iris.ONTOLAB: os.path.join(ONTOLOGY_DIR, 'OntoLab.owl'),
    iris.ONTOHPLC: os.path.join(ONTOLOGY_DIR, 'OntoHPLC.owl'),
    iris.ONTOVAPOURTEC: os.path.join(ONTOLOGY_DIR, 'OntoVapourtec.owl'),
}
