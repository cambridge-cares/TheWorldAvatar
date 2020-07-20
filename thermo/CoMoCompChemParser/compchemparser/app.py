from ontocompchemdata.ontocompchemdata import OntoCompChemData

def run(log_file):
    CompChemObj = OntoCompChemData()
    CompChemObj.getData(log_file)

    #parser.parse(log_file)
    pass
    #data = OntoCompChemdData()
    #data.getData(log_file)
