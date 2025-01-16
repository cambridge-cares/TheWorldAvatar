from SPARQL_Query_Wiki import SPARQL_Query_for_Wiki

query_wiki = SPARQL_Query_for_Wiki()
query = """# to get all instances under the class "structural class of chemical compounds"
SELECT DISTINCT ?item1 ?item2 ?item3
WHERE 
{
  OPTIONAL {?item1 wdt:P31 wd:Q183453 .}
  OPTIONAL{?item2 wdt:P31 wd:Q159226 .}
  OPTIONAL  {?item3 wdt:P31 wd:Q167377 .} 
}
 """
results = query_wiki.get_results(query)

for result in results["results"]["bindings"]:
    print(result)
