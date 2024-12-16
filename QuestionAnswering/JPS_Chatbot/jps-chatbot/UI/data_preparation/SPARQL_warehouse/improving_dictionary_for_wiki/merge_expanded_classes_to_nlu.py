# TODO: to expand the corpus and put them into the nlu file

import json

# In wiki_corpus, create_nlu_data_wiki.py

# the corpus is called  wiki_corpus/ corpus_for_trainning
# TODO: put the new classes into it
# TODO: put the SIMILES into it

# c_labels = labels[0]
# i_labels = labels[1]
# p_labels = labels[2]

with open('../../wiki_corpus/corpus_for_trainning') as f:
    labels = json.loads(f.read())
    c_labels = labels[0]
    i_labels = labels[1]
    p_labels = labels[2]

    counter_1 = 0
    counter_2 = 0
    # To load the expanded classes ...
    with open('expanded_class_dictionary') as f1:
        new_classes = json.loads(f1.read())
        print('number of new classes', len(new_classes))
        for key in new_classes:
            c = new_classes[key]

            label = c['label'].strip()
            if label not in c_labels:
                counter_1 = counter_1 + 1
                c_labels.append(label)
            alt_label = c['alt_label']
            for alt in alt_label:
                if alt not in c_labels:
                    counter_2 = counter_2 + 1
                    c_labels.append(alt.strip())

print('new label added to', counter_1)
print('new alt label added to', counter_2)
labels[0] = c_labels




counter_3 = 0
with open('smile_dict') as f2:
    smiles = json.loads(f2.read())
    # print(smiles)
    for k in smiles:
        s = smiles[k]
        # TODO: add smile to the trainning data (instances) ... DONE
        if s not in i_labels:
            i_labels.append(s.strip())
            counter_3 = counter_3 + 1

print('smiles added to', counter_3)
labels[1] = i_labels



with open('corpus_for_trainning_new', 'w') as f4:
    f4.write(json.dumps(labels))

