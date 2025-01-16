import re, json, time



unique_property_id = []
selected_properties = []
alt_counter = 0
with open('distinct_properties') as f:
    properties = json.loads(f.read())
    
    
    for property in properties: 
        print(property)
        id = re.findall(r'P[0-9]+', property['item']['value'])[0] 
        label = property['itemLabel']['value']      
        if 'itemAltLabel' in property:
            altLabels = property['itemAltLabel']['value'].strip().split(', ')
            alt_counter = alt_counter + len(altLabels)
        
        if id not in unique_property_id:
            unique_property_id.append(id)
            selected_properties.append(property)
            print(label) 
        # separate the id of the the property
        
    print('found ', len(unique_property_id), 'out of', len(properties))
    print('found', alt_counter, 'alt labels ')
    # found  245 labels 
    # found 3837 alt labels
 
with open('../wiki_corpus/selected_property', 'w') as f1:
    f1.write(json.dumps(selected_properties))
    f1.close()