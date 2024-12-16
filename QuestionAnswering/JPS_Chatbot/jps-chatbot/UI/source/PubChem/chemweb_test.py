endpoint = 'https://idsm.elixir-czech.cz/sparql/endpoint/idsm'


from SPARQLWrapper import SPARQLWrapper, JSON

sparql = SPARQLWrapper(endpoint)
sparql.setQuery("""
SELECT * WHERE
{
  ?S ?P ?O.
}
LIMIT 10
""")
sparql.setReturnFormat(JSON)
results = sparql.query().convert()

for result in results["results"]["bindings"]:
    print(result["label"]["value"])