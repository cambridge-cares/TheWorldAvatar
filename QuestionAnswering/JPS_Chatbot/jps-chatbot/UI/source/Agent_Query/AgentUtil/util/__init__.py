if __name__ == '__main__':
    from ModelLoader import ModelLoader, AGENT_NLU_MODEL
    from MarieLogger import *
    from StopWords import removeStopWords
    from SPARQLWarehouse import ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY, GET_AGENT_INPUT_PARAMETERS, GET_AGENT_OUTPUTS, GET_HTTP_URL
    from UniversalQuery import query_blazegraph
    from Lookup import *
    from location import FILE_DIR
    from SPARQLWarehouse import *

else:
    from .ModelLoader import ModelLoader
    from .MarieLogger import *
    from .StopWords import removeStopWords
    from .SPARQLWarehouse import ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY, GET_AGENT_INPUT_PARAMETERS, GET_AGENT_OUTPUTS, GET_HTTP_URL
    from .UniversalQuery import query_blazegraph
    from .Lookup import *
    from .location import FILE_DIR
    from .SPARQLWarehouse import *


