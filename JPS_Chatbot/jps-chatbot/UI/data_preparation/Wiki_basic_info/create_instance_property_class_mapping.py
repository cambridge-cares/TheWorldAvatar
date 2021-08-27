
import json, re, time, random
    



# get the instance-property mapping of the 5000 selected instances from wikidata

# EXTRA_PROPERTIES provide extra info for certain property e.g "geometry" for http://www.wikidata.org/entity/P117 (chemical structure)
from pprint import pprint

instances = []

 
with open('instance_property_mapping_first_2000') as f:
    d = json.loads(f.read())
    for i in d:
        print(len(d[i]['properties']))
    f.close()
           
with open('instance_property_mapping_random_3000') as f:
    json.loads(f.read())
    f.close()
            
with open('distinct_properties') as f:
    distinct_properties = json.loads(f.read())


# for each instance-property mapping, find the instance, get its labels, alt_labels, classes, and their labels and alt_labels 

# finally generate the instance-class mapping


        
 


