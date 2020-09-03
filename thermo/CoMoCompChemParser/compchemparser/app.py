from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData
from bz2 import __author__

from rdflib import Graph
from pathlib import Path

import random

def run(log_file,output_json,file_path):

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
        if file_path:
            if CompChemObj.data:
                CompChemObj.outputjson(True)
        elif CompChemObj.data:
             CompChemObj.outputjson(False)
             # CompChemObj.outputowl(ontocompchem_graph,file_name, r)
        else:
            print('No data to output/upload, check if log file is not empty or quantum job terminated correctly.')