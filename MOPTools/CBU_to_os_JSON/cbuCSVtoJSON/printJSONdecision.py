from kgoperations.querykg import querykg
from kgoperations.queryendpoints import SPARQL_ENDPOINTS
from kgoperations.querytemplate import spin_inchi_query

def printJSON(inchi_string, spin_number):
    result_1  = querykg(SPARQL_ENDPOINTS['ontospecies'], spin_inchi_query(inchi_string, spin_number)) 
    inchi_string = inchi_string.replace("InChI=1S/", "InChI=1/") # This replacement is introduced simply because two types of Inchi strings may exist in OntoSpecies
    result_2  = querykg(SPARQL_ENDPOINTS['ontospecies'], spin_inchi_query(inchi_string, spin_number)) 
    decision = False
    if result_1:
        if 'speciesIRI' in result_1[0].keys():
            if result_1[0]['speciesIRI'] is not None: 
                pass
    elif result_2:
        if 'speciesIRI' in result_2[0].keys():
            if result_2[0]['speciesIRI'] is not None: 
                pass
    else:
        decision = True
    return decision