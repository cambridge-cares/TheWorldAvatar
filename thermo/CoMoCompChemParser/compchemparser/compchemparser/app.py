from parsers import GaussianParser
from postprocessors import ArkanePostprocessor
from ontocompchemdata import OntoCompChemdData

GAUSSIAN = "gaussian"
#MOLPRO = 2

LOG_TYPES = [GAUSSIAN]

def correctLogType(log_type):
    ans = log_type in LOG_TYPES
    return ans

def run(log_file,log_type):
    postprocessor = ArkanePostprocessor()
    if log_type == GAUSSIAN:
        parser = GaussianParser()
    else:
        #parser = MolproParser()
        pass
    data = OntoCompChemdData()
    data.getData(log_file, parser, postprocessor)
    pass
