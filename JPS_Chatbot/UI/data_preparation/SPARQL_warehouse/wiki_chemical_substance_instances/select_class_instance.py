# iterate through all the files
# the name of the file is the id of the class 
# the content in the file are instances, containing the id, label, alt_labels

import os, re, json
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

# the length of the wikidata id to some extent indicates the importance of the instance
# e.g. Q11002 for CH4, Q2270 for Benzene and  1-{2-[5-(Furan-2-yl)-1-phenyl-4,5-dihydro-1H-pyrazol-3-yl]ethyl}piperidine--hydrogen chloride (1/1)
# is Q82876047. Therefore we need to find out the distribution of the length of id of the instances. 

def plot_histogram(data, bin, ylabel, xlabel):
    bin = len(list(dict.fromkeys(data)))
    plt.hist(data,bins=bin -1 )  # `density=False` would make counts
    plt.ylabel(ylabel)
    plt.xlabel(xlabel) 
    # plt.show()

def rank_instances(instances):
    # when several filter (e.g. the comprehensiveness of the node, the length of the label) are applied, 7439 instances are selected as the raw data set.
    # now it is necessary to further shrink the training set
    selected_instances = []
    selected_instances_dictionary = {4:[], 5:[], 6:[], 7:[]}
    length_of_id = []
    for instance in instances:
        id = re.search(r'Q[0-9]+', instance['item']['value'])[0]
        label_length = len(instance['itemLabel']['value'])
        # length_of_id.append(len(instance['itemLabel']['value']))
        length_of_id.append(len(id))
        if len(id) < 8:
            selected_instances.append(instance)
            selected_instances_dictionary[len(id)].append(instance) 
            
    plot_histogram(length_of_id, 10, 'y', 'x')   
    # base on the distribution of counts, we first try to remove ids longer than 8
    print('number of selected instances', len(selected_instances), 'out of', len(instances))
    # 2917 out of 7200 are selected 
    
    return selected_instances, selected_instances_dictionary
    
    
def rank_classes(classes):
    # plot distribution of the length of id of classes 
    length_of_id = []
    
    for _class in classes:
        length_of_id.append(len(_class))
    plot_histogram(length_of_id, 10, 'y', 'x')    
        
   
classes = []
valid_instances = []
length_of_label = []

instance_counter = 0 

for filename in os.listdir('./'):
    if re.match(r'Q[0-9]+', filename):
        classes.append(filename)
        with open(filename) as f:
            content = f.read()
            instances = json.loads(content)['results']['bindings']
            for instance in instances:
                instance_counter = instance_counter + 1
                label = (instance["itemLabel"]['value'])
                length_of_label.append(len(label))
                label_length = len(label)
                if label_length <= 19 and (not re.match(r'[0-9]', label)):
                    if instance not in valid_instances:
                        valid_instances.append(instance)

# 7200 out of 11310 are selected 
print(len(valid_instances), 'out of', instance_counter)

# plot_histogram(length_of_label, 140, 'Counts', 'Label lengths of chemical instancs')
# by plotting the distribution of the length of the labels, we notice that most of the valid labels are shorter than 25. Therefore, any label longer than 25 will be removed ... (You can't expect someone to type in a name that is over 25 characters in the questions ... )
valid_instances = rank_instances(valid_instances)
rank_classes(classes)

print('final result:', len(valid_instances[0]), 'instances selected out of', instance_counter)
print('             ', len(classes), 'classes selected out of', len(classes))

# 7200 out of 11310
# number of selected instances 2917 out of 7200
# final result: 2917 instances selected out of 11310
              # 412 classes selected out of 412.


with open('valid_instances', 'w') as f:
    f.write(json.dumps(valid_instances[0]))
    f.close()

class_counter = 0
selected_class_counter = 0

selected_classes = []
with open('all_distinct_classes') as f1:
    all_distinct_classes = json.loads(f1.read())
    for distinct_class in all_distinct_classes:
        class_counter = class_counter + 1
        id = re.search(r'Q[0-9]+', distinct_class['item'])[0]
        if id in classes:
            selected_class_counter = selected_class_counter + 1
            selected_classes.append(distinct_class)
    print('selected class', selected_class_counter, 'out of' ,class_counter)
    f1.close()
    
with open('../../wiki_corpus/selected_class', 'w') as f2:
    f2.write(json.dumps(selected_classes))
    f2.close()
    
    
with open('../../wiki_corpus/selected_instance', 'w') as f3:
    print('number of instances', len(valid_instances[0]))
    f3.write(json.dumps(valid_instances[0]))
    f3.close()
    
    
    
    
    
    
    
    
    
    
    
    
    

