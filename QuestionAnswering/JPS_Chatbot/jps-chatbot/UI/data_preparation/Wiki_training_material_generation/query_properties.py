from util.SPARQL_Query_Wiki import SPARQL_Query_for_Wiki
import json, re, time

SPARQL_template = '''
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

valuable_instances = []
property_names = []
list_of_properties = []
counter = 0
property_record = []
instance_property_mapping = {}

errors = []


# during the upgrade, the properties should be connected to instances
# a dictionary {'Qxxxxx': label, alt_labels,  [p1: label, alt_labels, p2, p3, p4]}
# later merge this dictionary with the instance-class dictionary
# which will serve as the ultimate corpus for trainning the NLU 


# get all the properties connected to the first 2000 high-value instances 
# then randomly choose another 3000 instances
# create the instance-property mapping 


def get_property(instance):
    id = re.search(r'Q[0-9]+', instance)[0]
    SPARQL_query = SPARQL_template % id
    try:
        single_list_property = []
        results = query_wiki.get_results(SPARQL_query)
        bindings = results['results']['bindings']
        for b in bindings:
            item = b['item']['value']
            if item in property_record:  # it already exists
                # then do nothing, it is repeated 
                pass
            else:
                # print('item', item)  # item is the URI of the property, itemLabel to be its label, and
                property_record.append(item)  # it is new, add it to the list
                if b not in list_of_properties:
                    list_of_properties.append(b)
                    print('collected', len(list_of_properties))
                    print(item)
            tmp = {}
            if 'item' in b:
                tmp['uri'] = b['item']['value']

            if 'itemLabel' in b:
                tmp['label'] = b['itemLabel']['value']

            if 'itemAltLabel' in b:
                altlabel = b['itemAltLabel']['value']
                tmp['alt_label'] = altlabel.split(',')

            if 'type' in b:
                tmp['type'] = b['type']['value']

            single_list_property.append(tmp)

        if id not in instance_property_mapping:
            instance_property_mapping[id] = {'properties': single_list_property}
    except:
        errors.append(id)
        with open('property_log', 'w') as f:
            f.write(json.dumps(errors) + '\n')
            f.close()
        pass
        time.sleep(0.5)


with open('distinct_properties') as f:
    list_of_properties = json.loads(f.read())
    f.close()

random_counter = 0
with open('WIKI_URI_LIST') as f:
    instances = json.loads(f.read())
    print('number of instances', len(instances))
    f.close()

# random_instance = random.sample(instances[2000:], 3000)
# get the properties of the first 2000 instances (ranked by their length, from short to long )
# random_instance = random.sample(instances[2000:], 200) + random.sample(instances[:2000], 200)



for instance in FAILED_CASES:

    counter = counter + 1
    print('iterated', counter, 'out of', len(FAILED_CASES))
    get_property(instance)

    if counter == 10:
        with open('ipm_test', 'w') as f:
            f.write(json.dumps(instance_property_mapping))
            f.close()
    with open('distinct_properties', 'w') as f:
        f.write(json.dumps(list_of_properties))
        f.close()
# with open('instance_property_mapping_first_2000', 'w') as f:
#     f.write(json.dumps(instance_property_mapping))
#     f.close()



# instance_property_mapping = {}
# for instance in random_instance:
#     random_counter = random_counter + 1
#     print('iterated', random_counter, 'out of 3000')
#
#     get_property(instance)
#
# with open('instance_property_mapping_random_3000', 'w') as f:
#     f.write(json.dumps(instance_property_mapping))
#     f.close()
#


##################################################
# randomly choose another
