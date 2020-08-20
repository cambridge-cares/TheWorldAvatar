from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData
from bz2 import __author__

from rdflib import Graph
from pathlib import Path

import random

def run(log_file,output_json):
    
    #r = random.uniform(100000,1000000)
    
    #file_name= Path(log_file).stem
     
    #create ontocompchem graph
    #ontocompchem_graph = Graph()
    
    # create OntoCompChemData object
    CompChemObj = OntoCompChemData()
    # parse the log, and once done upload data to KG
    # the upload function needs to be defined in the OntoCompChemData class
    CompChemObj.getData(log_file)
    #CompChemObj.uploadToKG()
    if output_json: 
         CompChemObj.outputjson()        
        # CompChemObj.outputowl(ontocompchem_graph,file_name, r)       