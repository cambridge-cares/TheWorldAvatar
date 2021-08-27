import json
import re

from UI.data_preparation.Wiki_basic_info.util.SPARQL_Query_Wiki import SPARQL_Query_for_Wiki

SPARQL_template_properties = '''
#All properties with descriptions and aliases and types
SELECT ?item ?type ?itemLabel ?itemAltLabel WHERE {
  wd:%s ?property ?x .
  ?item wikibase:directClaim ?property ;
        wikibase:propertyType ?type .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
GROUP BY  ?item ?type ?itemLabel ?itemAltLabel
'''

SPARQL_template_classes = '''
#All properties with descriptions and aliases and types
SELECT ?item ?type ?itemLabel ?itemAltLabel WHERE {
  wd:%s ?property ?x .
  ?item wikibase:directClaim ?property ;
        wikibase:propertyType ?type .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
GROUP BY  ?item ?type ?itemLabel ?itemAltLabel
'''


query_wiki = SPARQL_Query_for_Wiki()

with open('FULL_URI_LIST') as f:
    FULL_URI_LIST = json.loads(f.read())

with open('DISTINCT_PROPERTIES_AND_LABELS', 'w') as f2:
    f2.write('')
    f2.close()


FULL_URI_LIST = [U.replace('\n', '') for U in FULL_URI_LIST]
print(len(FULL_URI_LIST)) # 836875, all the species with SMIELS String
property_record = []
for URI in FULL_URI_LIST[:10]:
    id = re.search(r'Q[0-9]+', URI)[0]
    SPARQL_query = SPARQL_template_properties % id
    results = query_wiki.get_results(SPARQL_query)
    bindings = results['results']['bindings']
    for b in bindings:
        item = b['item']['value']
        if item in property_record:  # it already exists
            # then do nothing, it is repeated
            pass
        else:
            with open('DISTINCT_PROPERTIES_AND_LABELS', 'a') as f2:
                property_record.append(item)  # it is new, add it to the list
                f2.write('\n')
                f2.write(json.dumps(b))
