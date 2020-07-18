from parsers.ccparser import CcParser
from ontocompchemdata.ontocompchemdata import OntoCompChemData

def run(log_file):
    CompChemObj = OntoCompChemData()
    CompChemObj.setParser(CcParser())
    CompChemObj.getData(log_file)

    parser.parseLog(log_file)
    pass
    #data = OntoCompChemdData()
    #data.getData(log_file)
