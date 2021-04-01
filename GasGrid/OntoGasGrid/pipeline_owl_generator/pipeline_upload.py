from py4jps.resources import JpsBaseLib
import time
import os 
from tqdm import tqdm 
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST

# The purpose of this module is to create and start resource
# gateway objects to be used in all your other modules
#============================================================
from py4jps.resources import JpsBaseLib

# jpsBaseLib resource gateway
# you may also wish to pass any **JGkwargs
jpsGW = JpsBaseLib()

# you may also wish to pass any **LGkwargs
jpsGW.launchGateway()

jpsGW_view = jpsGW.createModuleView()
jpsGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")

KGRouter = jpsGW_view.KGRouter


def owl2KG(kgURL=None, namespace=None, owlSrcDir=None, rFlag=False):
    if kgURL is None: kgURL = params.REMOTE_KG if rFlag else params.LOCAL_KG
    if namespace is None: namespace = params.DEF_NAMESPACE
    if owlSrcDir is None: owlSrcDir = params.OWL_SRC_DIR

    KnowledgeRepository = jpsGW_view.KnowledgeRepository(kgURL, namespace,"",owlSrcDir)

    KnowledgeRepository.uploadOntologies()

owl2KG('http://192.168.1.12:9999/bigdata','ontogasgrid','/Users/tomsavage/Documents/TheWorldAvatar/GasGrid/OntoGasGrid/abox-tbox/pipeline_abox/')