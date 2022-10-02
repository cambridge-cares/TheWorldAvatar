from pubchem.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchem.kgoperations.querykg import querykg, insertkg
from pubchem.kgoperations.inserttemplates import test_data_insert
import pubchem.unitconverter.unitconverter as unitconv



def insert_test_data():
    query = test_data_insert()
    sparqlendpoint = SPARQL_ENDPOINTS['copyontospecies']
    data = insertkg(sparqlEndPoint=sparqlendpoint, queryStr=query)


    if data:
        data = data[0]

    return data


    
    