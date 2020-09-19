from util.SPARQL_Query_Wiki import SPARQL_Query_for_Wiki
import json, re, time

SPARQL_template = '''
#All properties with descriptions and aliases and types
SELECT ?item ?itemLabel  ?itemAltLabel WHERE {
  wd:%s ?property ?x .
  ?item wikibase:directClaim ?property .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
GROUP BY  ?item ?itemLabel ?itemAltLabel
'''
query_wiki = SPARQL_Query_for_Wiki()

valuable_instances = []
property_names = []
list_of_properties = []
counter = 0

with open('wiki_chemical_substance_instances/valid_instances') as f:
    instances = json.loads(f.read())[1]
    valuable_instances = valuable_instances + instances['4'] + instances['5'] + instances['6']
    print('number of valuable instances', len(valuable_instances))
    for instance in valuable_instances:
        counter = counter + 1
        print('iterated', counter, 'out of', len(valuable_instances))
        full_id = instance['item']['value']
        label = instance['itemLabel']['value']       
        print(label)
        id = re.search(r'Q[0-9]+', full_id)[0]
        SPARQL_query = SPARQL_template % id 
        print('----------------------')
        try:
            results = query_wiki.get_results(SPARQL_query)
            print(results)
            list_of_properties = list_of_properties + results["results"]["bindings"]
            print('collected', len(list_of_properties))
        except:
            pass        
        time.sleep(1)
        
with open('distinct_properties', 'w') as f:
    f.write(json.dumps(list_of_properties))
    f.close()


