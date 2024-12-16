import json

# TODO: collect the label and alt_label of the classes ... which will be later put into Wiki_dictionary
import sys
import time
from pprint import pprint

from SPARQLWrapper import SPARQLWrapper, JSON

with open('expanded_class_list') as f:
    data = json.loads(f.read())

SPARQL_TEMPLATE = '''
SELECT   ?label (GROUP_CONCAT(DISTINCT(?altLabel); separator = "$") AS ?altLabel_list)
WHERE
{
    wd:%s rdfs:label ?label .
    FILTER (langMatches( lang(?label), "EN" ) )
    OPTIONAL { wd:%s skos:altLabel ?altLabel . FILTER (lang(?altLabel) = "en") }
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en" .}
  
}GROUP BY ?label

'''

expanded_class_dictionary = {}


def get_results(query):
    endpoint_url = "https://query.wikidata.org/sparql"
    user_agent = "WDQS-example Python/%s.%s" % (sys.version_info[0], sys.version_info[1])
    sparql = SPARQLWrapper(endpoint_url, agent=user_agent)
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    return sparql.query().convert()


data = list(set(data))
# data = data[:5]

total_length = len(data)
counter = 0
start_time = time.time()

print(len(data))  # 75 classes,
for id in data:
    counter = counter + 1
    uri = id
    id = id.replace('http://www.wikidata.org/entity/', '')
    print(id)
    query = SPARQL_TEMPLATE % (id, id)
    # print(query)
    bindings = get_results(query)['results']['bindings']
    expanded_class_dictionary[uri] = {'label': '', 'alt_label': []}
    if len(bindings) > 0:
        d = bindings[0]
        if 'label' in d:
            label = d['label']['value']
            print(label)
            expanded_class_dictionary[uri]['label'] = label
        if 'altLabel_list' in d:
            if d['altLabel_list']['value'] is not "":
                alt_label = d['altLabel_list']['value'].split('$')
                expanded_class_dictionary[uri]['alt_label'] = alt_label

    print(counter, ' out of ', total_length)
    time_taken = round(time.time() - start_time)
    print(time_taken, 'seconds taken')
    estimated_time = time_taken / counter * (total_length - counter)
    print(round(estimated_time / 60, 2), 'mins left')
    print('-------------')
    time.sleep(2)

pprint(expanded_class_dictionary)

with open('expanded_class_dictionary', 'w') as f:
    f.write(json.dumps(expanded_class_dictionary))
