from SPARQLWrapper import SPARQLWrapper, JSON
from Marie.Util.Web.SPARQLWarehouse import ONTOCOMPCHEM_HANDSHAKE, PUBCHEM_HANDSHAKE


def handshake(namespace="ontocompchem", query=ONTOCOMPCHEM_HANDSHAKE):
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    return results


def handshake_ontocompochem():
    try:
        result = handshake(namespace="ontocompchem", query=ONTOCOMPCHEM_HANDSHAKE)
        result = result["results"]["bindings"]
        if len(result) == 10:
            return True
        else:
            return False
    except:
        return False


def handshake_pubchem():
    try:
        result = handshake(namespace="CleanPubChem", query=PUBCHEM_HANDSHAKE)
        result = result["results"]["bindings"]
        if len(result) == 10:
            return True
        else:
            return False
    except:
        return False

