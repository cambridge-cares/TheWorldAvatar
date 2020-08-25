import json, re, time, random
import nltk

# returns all_c_labels, all_i_labels, all_p_labels
def further_process_labels():

    all_p_labels = []   # stores the labels of all properties
    all_i_labels = []   # stores the labels of all instances
    all_c_labels = []   # stores the labels of all concepts

    original_alt_p_count = 0
    alt_properties_count = 0 

    
    # =================== further process the properties and their labels =========
    with open('selected_property') as f0:
        properties = json.loads(f0.read())
        trimmed_property = []
        for property in properties:
            p_with_trimmed_alt = {'item': property['item']['value']}
            p_with_trimmed_alt['itemLabel'] = property['itemLabel']['value']
            if property['itemLabel']['value'] not in all_p_labels:
                all_p_labels.append(property['itemLabel']['value'])
            if 'itemAltLabel' in property:
                alt_labels = property['itemAltLabel']['value'].split(', ')
                original_alt_p_count = original_alt_p_count + len(alt_labels)
                shorter_alt_labels = [x.strip() for x in alt_labels if not ((len(x) > 35) or (' ID' in x)  or (not re.fullmatch(r'[a-zA-Z ]+', x)))]
                for lb in shorter_alt_labels:
                    if ' ID' in lb:
                        print(lb)
                        input()


                for l in shorter_alt_labels:
                    if l.lower() not in all_p_labels:
                        all_p_labels.append(l)
                
                p_with_trimmed_alt['itemAltLabel'] = shorter_alt_labels
                # print(p_with_trimmed_alt['itemAltLabel'])
                alt_properties_count = alt_properties_count + len(shorter_alt_labels)
            trimmed_property.append(p_with_trimmed_alt)    
                
    print('count of shorter_alt_labels', alt_properties_count)
    print('all original_alt_p_count', original_alt_p_count)
    print('all properties', len(properties))

    # print(trimmed_property)
            
    # at the last step, we are looking at 245 property labels and 359 alt labels
    
    with open('properties_short_label', 'w') as file0:
        file0.write(json.dumps(trimmed_property))
        file0.close()
    
    # =============================================================================

    original_alt_i_count = 0
    alt_instance_count = 0 

    # =============== further process the instances and their labels ==============
   
    with open('selected_instance') as f1:
        instances = json.loads(f1.read())
        trimmed_instances = []
        for instance in instances:
            i_with_trimmed_alt = {'item': instance['item']['value']}
            i_with_trimmed_alt['itemLabel'] = instance['itemLabel']['value']
            if instance['itemLabel']['value'].lower() not in all_i_labels:
                all_i_labels.append(instance['itemLabel']['value'])
            if 'altLabel_list' in instance:
                alt_labels = instance['altLabel_list']['value'].split('$ ')
                original_alt_i_count = original_alt_i_count + len(alt_labels)

                shorter_alt_labels = [x.strip() for x in alt_labels if not ((len(x) > 30) or (' ID' in x) or (not re.fullmatch(r'[a-zA-Z ]+', x)))]
                
                for l in shorter_alt_labels:
                    if l.lower() not in all_i_labels:
                        all_i_labels.append(l.lower())
                # also include the chemical formula for instances 
                SUB = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789" )
                chemical_formula = instance['x']['value']
                chemical_formula = chemical_formula.translate(SUB)
                all_i_labels.append(chemical_formula)
                
                i_with_trimmed_alt['itemAltLabel'] = shorter_alt_labels
                # print(i_with_trimmed_alt['itemAltLabel'])
                alt_instance_count = alt_instance_count + len(shorter_alt_labels)
            trimmed_instances.append(i_with_trimmed_alt)    

    # print('count of shorter_alt_labels', alt_instance_count)
    # print('all original_alt_i_count', original_alt_i_count)     
    # print('all distinct alt labels', len(all_i_labels)) 
    # print('all instances', len(instances))        
    with open('instances_short_label', 'w') as file1:
        file1.write(json.dumps(trimmed_instances))
        file1.close()

    # ================= further process the classes and their labels =============
    
    with open('selected_class') as f2:
        classes = json.loads(f2.read())
        for c in classes:
            label = c['itemLabel'].strip()
            if label.lower() not in all_c_labels:
                all_c_labels.append(label)
    
    
    return all_c_labels, all_i_labels, all_p_labels
        



labels = further_process_labels()
c_labels = labels[0]
i_labels = labels[1]
p_labels = labels[2]

with open('corpus_for_trainning', 'w') as f:
    f.write(json.dumps(labels))
    f.close()




