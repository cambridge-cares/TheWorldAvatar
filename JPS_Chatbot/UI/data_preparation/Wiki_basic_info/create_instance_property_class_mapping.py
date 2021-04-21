
import json, re, time, random
    



# get the instance-property mapping of the 5000 selected instances from wikidata

# extra_properties provide extra info for certain property e.g "geometry" for http://www.wikidata.org/entity/P117 (chemical structure)



 
with open('instance_property_mapping_first_2000', 'w') as f:
    f.write(json.dumps(instance_property_mapping))
    f.close()
           
with open('instance_property_mapping_random_3000', 'w') as f:
    f.write(json.dumps(instance_property_mapping))
    f.close()
            
   
   
# for each instance-property mapping, find the instance, get its labels, alt_labels, classes, and their labels and alt_labels 

# finally generate the instance-class mapping


        
 


