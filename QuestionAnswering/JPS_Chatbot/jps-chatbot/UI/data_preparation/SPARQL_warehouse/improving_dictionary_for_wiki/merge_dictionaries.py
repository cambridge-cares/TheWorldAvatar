# TODO: load the old dictionary, select out the part of classes ...
# TODO: the smiles need to go to the dictionary as well
# The file is : ./source/search_engine/wiki_dictionary'
import json

with open('../../source/search_engine/wiki_dictionary') as wiki_dictionary_file:
    old_dictionary = json.loads(wiki_dictionary_file.read())
    classes = old_dictionary['class']
    instances = old_dictionary['entity']
    dict_class = classes['dict']
    list_class = classes['list']

# load the new dictionary ...

with open('expanded_class_dictionary') as new_dictionary_file:
    new_dictionary = json.loads(new_dictionary_file.read())

counter = 0
counter_2 = 0

new_class_list = list_class
new_instance_list = instances['list']

with open('smile_dict') as f0:
    smile_dictionary = json.loads(f0.read())
    for k in smile_dictionary:
        smile = smile_dictionary[k].strip()
        print(smile)
        if smile not in new_instance_list:
            new_instance_list.append(smile)
            instances['dict'][smile] = ['http://www.wikidata.org/entity/' + k]




for uri in new_dictionary:

    # # The targets are 'diamine', 'aromatic hydrocarbon '
    obj = new_dictionary[uri]
    # print(obj)
    label = obj['label']  # the label is a string

    # ================== expand the list of words ================
    if label not in new_class_list:
        counter = counter + 1
        new_class_list.append(label.strip())
        # put this word into the dictionary
        if label in dict_class: # then we have a problem
            print('Then we have a problem')
            print(dict_class[label]) # we dont have this problem
        else:
            dict_class[label] = [('http://www.wikidata.org/entity/' + uri).strip()]
            # old_dictionary[label].append()

for uri in new_dictionary:
    obj = new_dictionary[uri]
    alt_label = obj['alt_label']  # the alt label is a list of strings, maybe empty
    for alt in alt_label:
        if alt not in new_class_list:
            counter_2 = counter_2 + 1
            new_class_list.append(alt.strip())
            if alt in dict_class:  # then we have a problem
                print('Then we have a problem')
                print(dict_class[alt])  # we dont have this problem
            else:
                dict_class[alt] = [('http://www.wikidata.org/entity/' + uri).strip()]

    # ================== expand the dictionary =======================

# re-assign the values
# dict_class = classes['dict']
# list_class = classes['list']

old_dictionary['class']['dict'] = dict_class
old_dictionary['class']['list'] = new_class_list

old_dictionary['entity']['dict'] = instances['dict']
old_dictionary['entity']['list'] = new_instance_list

print(old_dictionary['class']['dict']['diamine'])
print(old_dictionary['class']['dict']['aromatic hydrocarbon'])
print(old_dictionary['class']['dict']['polyamide'])

print(old_dictionary['entity']['dict']['CC=O'])



print('added', counter, 'new class labels')
print('added', counter_2, 'new class alt labels')


# write the file, generate a new wiki_dictionary_new
with open('wiki_dictionary_new', 'w') as f:
    f.write(json.dumps(old_dictionary))
    f.close()


#

# for c in new_class_list:
#
#     if 'diamine' in c:
#         print('we found diamine')
#         print(c)
#
#     if 'aromatic hydrocarbon' in c:
#         print('we found aromatic hydrocarbon')
#         print(c)



# TODO: include the smile identifier into the dictionary ...
