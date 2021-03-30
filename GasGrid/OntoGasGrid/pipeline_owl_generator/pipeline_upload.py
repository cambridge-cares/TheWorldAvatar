from py4jps.resources import JpsBaseLib
import time
import os 
from tqdm import tqdm 
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST

def dir_upload(dir):
    for filename in tqdm(os.listdir(dir)):
        if filename.endswith('.owl'):
            query = 'LOAD <file://'+dir+filename+'>'
            DEF_NAMESPACE = 'ontogasgrid'
            LOCAL_KG = "http://localhost:9999/bigdata"
            LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
            sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
            sparql.setMethod(POST) # POST query, not GET
            sparql.setQuery(query)
            sparql.query()
    return

dir_upload('/Users/tomsavage/Documents/TheWorldAvatar/GasGrid/OntoGasGrid/abox-tbox/pipeline_abox/')