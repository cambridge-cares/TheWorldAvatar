from util.SPARQL_Query_Wiki import SPARQL_Query_for_Wiki
import json, re, time
from os import listdir
from os.path import isfile, join


SPARQL_template = '''
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
SELECT DISTINCT ?label  (GROUP_CONCAT(DISTINCT(?altLabel); separator = "$") AS ?altLabel_list) ?formula ?class ?classLabel 
WHERE 
{
    wd:%s   rdfs:label ?label .
    FILTER (lang(?label) = "en") 
    OPTIONAL { wd:%s    skos:altLabel ?altLabel ;
                        wdt:P274 ?formula ;
                         wdt:P31/wdt:P279* ?class .
              FILTER (lang(?altLabel) = "en") }
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en" .}
}
GROUP BY ?label ?class ?classLabel ?formula 
'''
query_wiki = SPARQL_Query_for_Wiki()

distinct_classes = []

# with open('query_log', 'w') as f:
    # f.write(str(time.time()) + '\n')
    # f.close()


mypath = 'D:/data/instance_info'
existing_files = [f for f in listdir(mypath) if isfile(join(mypath, f)) and f.startswith('Q')]



counter = len(existing_files)
starting_point = counter
start_time = time.time()
# with open('WIKI_URI_LIST') as f:
#     FULL_LIST = json.loads(f.read())

with open('query_log') as f:
    FAILED_CASES = [c.strip() for c in f.readlines() if 'http://www.wikidata.org/entity' in c]

with open('FULL_URI_LIST') as f:
    FULL_URI_LIST = f.readlines()
    # instances = json.loads(f.read())
    instances = FAILED_CASES
    print('number of instances', len(instances))
    for instance in instances:
        id = re.search(r'Q[0-9]+', instance)[0]
        if id in existing_files:
            pass
        
        else:
    
            counter = counter + 1
            print('iterated', counter, 'out of', len(FULL_URI_LIST))
            time_used = time.time() - start_time
            print('already took', round(time_used, 2))
            ETA = ((time_used/(counter - starting_point)) * (len(instances) - counter) / 60)
            print('ETA',ETA , 'minutes')
            print('   ', ETA / 60, 'hours')
            SPARQL_query = SPARQL_template % (id, id)
            try:
                results = query_wiki.get_results(SPARQL_query)
                with open('D:/data/instance_info/%s' % id, 'w') as f:
                    f.write(json.dumps(results, indent=4))
                    f.close()
                # bindings = results['results']['bindings']
                # for b in bindings:
                #     if 'class' in b:
                #         class_uri = b['class']['value']
                #         if 'classLabel' in b:
                #             class_label = b['classLabel']['value']
                #
                #             if class_uri in distinct_classes: # it already exists
                #                 # then do nothing, it is repeated
                #                 pass
                #             else:
                #                 tmp = {class_uri: class_label}
                #                 distinct_classes.append(tmp)
                #                 with open('distinct_classes', 'w') as f:
                #                     f.write(json.dumps(distinct_classes))
                #                     f.close()
                         
            except Exception:
                with open('query_log', 'a') as f:
                    print('ERROR',Exception )
                    print(instance)
                    f.write(instance + '\n')
                    f.close()
                pass        
            time.sleep(0.01)
    




