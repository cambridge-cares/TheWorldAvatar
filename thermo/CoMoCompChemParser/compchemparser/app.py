from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData

def run(log_file):
    # create OntoCompChemData object
    CompChemObj = OntoCompChemData()
    # parse the log, and once done upload data to KG
    # the upload function needs to be defined in the OntoCompChemData class
    CompChemObj.getData(log_file)
    #CompChemObj.uploadToKG()
