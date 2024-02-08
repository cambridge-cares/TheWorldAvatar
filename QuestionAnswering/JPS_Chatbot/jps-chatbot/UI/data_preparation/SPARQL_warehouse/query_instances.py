import sys

from util.SPARQL_Query_Wiki import SPARQL_Query_for_Wiki
import json,re,time
query_wiki = SPARQL_Query_for_Wiki()

SPARQL_template = '''SELECT ?item ?itemLabel ?x (GROUP_CONCAT(DISTINCT(?altLabel); separator = "$ ") AS ?altLabel_list)
WHERE
{
  ?item wdt:P31 wd:%s ;
        wdt:P274 ?x ;
        wdt:P117 ?y ;
        wdt:P231 ?xx  ;
        wdt:P2067 ?z .
  
    OPTIONAL { ?item skos:altLabel ?altLabel . FILTER (lang(?altLabel) = "en") }
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en" .}
  
}GROUP BY ?item ?itemLabel ?x
'''

failed_ones = []

with open('all_distinct_classes') as f:
    list = json.loads(f.read())

counter = 0
number_of_all_classes = len(list)
number_of_non_empty_ones = 0
number_of_failed_ones = 0

start_time = time.time()

for item in list:
    counter = counter + 1
    id = re.findall(r'Q[0-9]+', item['item'])[0] # separate the id of the classes. 
    SPARQL_query = SPARQL_template % id 
    print('----------------------')
    try:
        results = query_wiki.get_results(SPARQL_query)
        print('the length to be: ', len(results["results"]["bindings"]))
        if len(results["results"]["bindings"]) != 0:
            number_of_non_empty_ones = number_of_non_empty_ones + 1
            with open(id, 'w') as f:
                f.write(json.dumps(results))
    except:
        failed_ones.append(id)
        number_of_failed_ones = number_of_failed_ones + 1
        
    print('number_of_non_empty_ones', number_of_non_empty_ones, 'out of ', number_of_all_classes)
    print('number_of_attempts', counter, 'progress', str(counter/ number_of_all_classes * 100),'%')
    print('time consumed', time.time() - start_time)
    print('number_of_failed_ones',number_of_failed_ones )
    time.sleep(1)
    
with open('failed_ones', 'w') as f:
    f.write(json.dumps(failed_ones))
    f.close()