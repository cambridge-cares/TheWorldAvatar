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
property_record = []

with open('WIKI_URI_LIST') as f:
    instances = json.loads(f.read())
    print('number of instances', len(instances))
    for instance in instances:
        counter = counter + 1
        if counter == 500:
            with open('distinct_properties', 'w') as f:
                f.write(json.dumps(list_of_properties))
                f.close()
                
            exit()
        print('iterated', counter, 'out of', len(instances))
        id = re.search(r'Q[0-9]+', instance)[0]
        SPARQL_query = SPARQL_template % id 
        try:
            results = query_wiki.get_results(SPARQL_query)
            bindings = results['results']['bindings']
            for b in bindings:
                item = b['item']['value']
                if item in property_record: # it already exists 
                    # then do nothing, it is repeated 
                    pass
                else:
                    print('item', item)
                    property_record.append(item) # it is new, add it to the list 
                    list_of_properties.append(b)
                    print('collected', len(list_of_properties))
        except:
            pass        
        time.sleep(1)
    
    



