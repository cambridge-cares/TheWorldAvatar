from SPARQLWrapper import SPARQLWrapper, JSON


def query_blazegraph(query, namespace="ontospecies"):
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    return results