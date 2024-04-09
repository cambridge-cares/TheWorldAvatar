# TODO: go to the selected_instance
# TODO: rank them somehow, by URI instance of, subclass of ...
# TODO: find the subclasses of the subclass ... and the way reversed

# The targets are 'diamine', 'aromatic hydrocarbon '

# Rank the instances
import json
import sys
import time

from SPARQLWrapper import SPARQLWrapper, JSON

from pprint import pprint


SPARQL_template = '''
# To get the smiles, to query the class and the subclasses of an instance 
SELECT ?smiles  
(GROUP_CONCAT(DISTINCT(?class); separator = ",") AS ?class_list) 
(GROUP_CONCAT(DISTINCT(?subclass); separator = ",") AS ?subclass_list)
#(GROUP_CONCAT(DISTINCT(?class_label); separator = ",") AS ?class_label_list)
#(GROUP_CONCAT(DISTINCT(?subclass_label); separator = ",") AS ?subclass_label_list)
 
WHERE 
{
  wd:%s wdt:P31 ?class .
  OPTIONAL {
  wd:%s wdt:P279  ?subclass . 
  wd:%s wdt:P233 ?smiles . 
  
  }
  # ?class  rdfs:label ?class_label .
  # FILTER (langMatches( lang(?class_label), "EN" ) )
  # ?subclass  rdfs:label ?subclass_label .
  # FILTER (langMatches( lang(?subclass_label), "EN" ) )
   
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}GROUP BY  ?smiles 
'''



def get_results(query):
    endpoint_url = "https://query.wikidata.org/sparql"
    user_agent = "WDQS-example Python/%s.%s" % (sys.version_info[0], sys.version_info[1])
    sparql = SPARQLWrapper(endpoint_url, agent=user_agent)
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    return sparql.query().convert()

all_distinct_classes = []
with open('../selected_instance') as f:
    instances = json.loads(f.read())
    print('The number of instances concerned', len(instances))
    newlist = sorted(instances, key=lambda x: len(x['item']))
    newlist.insert(0, {'item': {'value': 'Q2270'}})

# newlist = newlist[:5]
total_length = len(newlist)
counter = 0
start_time = time.time()

SMILE_DICTIONARY = {}

for item in newlist:
    counter = counter + 1
    instance_id = item['item']['value'].replace('http://www.wikidata.org/entity/', '')
    # print(instance_id)
    SPARQL_query = SPARQL_template % (instance_id,instance_id,instance_id)
    print('---------------')
    # print(SPARQL_query)
    r = get_results(SPARQL_query)
    bindings = r['results']['bindings']
    # print(bindings)
    if len(bindings) > 0:
        data = bindings[0]
        print(data)
        class_list = data['class_list']['value']
        if class_list is not '':
            all_distinct_classes = all_distinct_classes + class_list.split(',')
        subclass_list = data['subclass_list']['value']
        if subclass_list is not '':
            all_distinct_classes = all_distinct_classes + subclass_list.split(',')
        if 'smiles' in data:
            smile = (data['smiles']['value'])
            SMILE_DICTIONARY[instance_id] = smile


        # print(class_list)
        # print(subclass_list)
        # print('----------------')
    print(counter, ' out of ', total_length)
    time_taken = round(time.time() - start_time)
    print(time_taken, 'seconds taken')
    estimated_time = time_taken / counter * (total_length - counter)
    print(round(estimated_time / 60,2), 'mins left')
    time.sleep(2)


# The targets are 'diamine', 'aromatic hydrocarbon'

# pprint(set(all_distinct_classes))
with open('expanded_class_list', 'w') as f:
    f.write(json.dumps(list(set(all_distinct_classes))))

with open('smile_dict', 'w') as f1:
    f1.write(json.dumps(SMILE_DICTIONARY))