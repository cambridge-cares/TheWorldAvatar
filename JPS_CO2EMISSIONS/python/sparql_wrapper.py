from SPARQLWrapper import SPARQLWrapper, JSON, POST

def sparqlQueryRead(queryString):
#     sparql = SPARQLWrapper("http://localhost:8081/fuseki/test/sparql")
    sparql = SPARQLWrapper("http://www.theworldavatar.com/damecoolquestion/worldpowerplantsng/sparql")
    sparql.setQuery(queryString)
    sparql.setReturnFormat(JSON)

    return sparql.query().convert()

def sparqlQueryWrite(queryString):
#     sparql = SPARQLWrapper("http://localhost:8081/fuseki/test/update")
    sparql = SPARQLWrapper("http://www.theworldavatar.com/damecoolquestion/worldpowerplantsng/update")
    sparql.setQuery(queryString)
    sparql.setMethod(POST)
    return sparql.query()