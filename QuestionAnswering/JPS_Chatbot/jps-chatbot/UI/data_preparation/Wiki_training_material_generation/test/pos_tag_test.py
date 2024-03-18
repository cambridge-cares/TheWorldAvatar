import json, re

import nltk

from nltk.stem import PorterStemmer
from datetime import date

ps = PorterStemmer()

skip_tags = ['NN : NN', 'WRB', 'VBZ ( NN )', 'NN ( JJ NNS )', 'RB JJ TO']
def join_tags(tags_words):
    tmp = {}
    if len(tags_words) == 2 or len(tags_words) == 3:
        for i in range(len(tags_words)):
            tmp.update(tags_words[i])
        return tmp


unhandled_tag_chain = []


def get_pos_tags(p_label):
    stop_tags = ['DT']
    text = nltk.word_tokenize(p_label)
    # text = [ ps.stem(w.lower()) for w in text]
    word_tag_pairs = nltk.pos_tag(text)
    tags = [pair[1] for pair in word_tag_pairs if pair[1] not in stop_tags]
    tags_words = [{pair[1]: pair[0]} for pair in word_tag_pairs if pair[1] not in stop_tags]
    tag_chain = ' '.join(tags)

    if 'upper flamm' in p_label:
        print(p_label)
        print(tag_chain)
        x = input()


    if tag_chain.endswith(' IN'):
        handled_property.append(p_label)
        # if it starts with VB, then it is a different story
        if tag_chain == 'VBN IN':
            # stem the VBN
            # split the two element attribute into 'VBN': created, 'IN': in, 'VB':
            tag_dict = join_tags(tags_words)
            return tag_dict
        elif tag_chain == 'NN IN':
            return

        elif tag_chain == 'VBZ NN IN':
            tag_dict = join_tags(tags_words)

        elif tag_chain == 'JJ IN':
            pass
        elif tag_chain == 'NNS IN':
            pass
        elif tag_chain == 'VBN IN IN':
            pass

        elif tag_chain == 'JJ NN IN':
            pass
        elif tag_chain.startswith('VBZ'):
            pass
        elif tag_chain == 'NN IN NN':
            pass
        else:
            pass

    elif tag_chain == 'NN' or tag_chain == 'NN NN' or tag_chain == 'NN NN NN':
        handled_property.append(p_label)

        # print('----------------')
        # print(p_label.strip())
        # print(tag_chain)
        # tag_dict = join_tags(tags_words)
        # print(tag_dict)
        pass
    elif tag_chain == 'NN IN NN':
        handled_property.append(p_label)

        pass
    elif tag_chain == 'VBN':
        handled_property.append(p_label)
        # p_label = ps.stem(p_label.lower())

    elif tag_chain == 'JJ NN' or tag_chain == 'VBG NN':
        # atomic number
        # ultimate cause
        # magnetic moment
        # defining parameter
        # kinematic viscosity
        # electrical resistivity
        print(tag_chain)
        print(p_label)
        print('--------------')
        handled_property.append(p_label)

    elif tag_chain == 'NNS':
        # treats
        # contains
        # implies
        handled_property.append(p_label)

    elif tag_chain == 'VBZ NN':
        # has reason
        # has branch
        # has superclass
        # has characteristic
        handled_property.append(p_label)

    elif tag_chain == 'VBN IN NN':
        # described in source
        # described by source
        # described by biography
        # found in taxon
        handled_property.append(p_label)
        # IN what NN is I VBN

    elif tag_chain == 'VBZ JJ':
        # is an individual
        # is a particular
        # is a specific
        # is a unique
        handled_property.append(p_label)

    elif tag_chain == 'IN':
        # as
        # within
        # because
        # in
        handled_property.append(p_label)

    elif tag_chain == 'NNS NN':
        # Commons gallery
        # Commons category
        # Commons image
        handled_property.append(p_label)

    elif tag_chain == 'VBZ':
        # is a
        # is an
        # includes
        handled_property.append(p_label)
    elif tag_chain == 'VBZ RB':
        # is also a
        # isn't
        # is not
        handled_property.append(p_label)

    elif tag_chain == 'RBR JJ NN':
        handled_property.append(p_label)


    elif tag_chain in ['VBN IN NNS', 'VBD NN', 'VBN IN NN NN', 'VB NN','NN IN NNS', 'NNP NNP','JJ NN CC JJ NN NN', 'NN CC NN', 'NN IN NN CC NN', 'NN NNS', 'NNP NNP NNP NNP','RB','NNP PRP NN', 'NN IN NNP NNP']:
        # discoverer or inventor
        # had cause
        # described by reference work
        # time of discovery or invention
        # NIOSH Pocket Guide ID
        # number of protons
        # molecular model or crystal lattice model
        # Unicode hex codepoint
        # have part
        # inventor or discoverer
        handled_property.append(p_label)
    elif tag_chain == 'VBG':
        # drawing
        handled_property.append(p_label)

    elif tag_chain == 'JJ NNP':
        # isomeric SMILES
        # canonical SMILES
        handled_property.append(p_label)
    elif tag_chain == 'NNP NN':
        # Unicode symbol
        # Unicode character
        handled_property.append(p_label)
    elif tag_chain in ['JJ JJ NN NN', 'JJ NN NN']:
        # time-weighted average exposure limit
        # short-term exposure limit
        # acid dissociation constant
        handled_property.append(p_label)
    elif tag_chain in ['JJ NN VBD', 'NN VBD', 'NNS VBP']:
        # medical condition treated
        # treats disease
        # date discovered
        # disease treated
        handled_property.append(p_label)
    elif tag_chain == 'NNS TO':
        # decays to
        handled_property.append(p_label)
    elif tag_chain in ['RB JJ TO NN CC NN', 'NN IN JJ', 'NN IN NN NN', 'NN NN CC NN', 'NN POS JJ NN', 'NNP NN NN', 'NNP NNP NNP NNP NNP NNP']:
        #  image of exterior
        #  immediately dangerous to life or health
        #  Simplified Molecular Input Line Entry Specification
        #  safety classification and labelling
        handled_property.append(p_label)
    elif tag_chain in ['VBZ NNS', 'VBN NN', 'VBZ NN NN', 'VBZ NNS']:
        #  has underlying cause
        #  has conjugate base
        #  has causes
        #  has ingredients
        handled_property.append(p_label)
    elif tag_chain == '':
        pass
    else:
        if tag_chain in skip_tags:
            pass
        else:
            unhandled_tag_chain.append(tag_chain)
            pass

    # else:
    # print()
    # print(tag_chain)

    # rearrange the elements in the


get_pos_tags('upper flammable limit')


handled_property = []
with open('../distinct_properties') as f:
    DISTINCT_PROPERTIES = json.loads(f.read())

print(len(DISTINCT_PROPERTIES))

all_distinct_labels = []
for p in DISTINCT_PROPERTIES:
    label = p['itemLabel']['value']
    p_type = p['type']['value']
    if 'ExternalId' in p_type:  # filter out external id properties
        pass
    else:
        if 'itemAltLabel' in p:
            alt_label = p['itemAltLabel']['value'].split(',')
            all_labels = list(set([label] + alt_label))
        else:
            all_labels = [label]
        all_distinct_labels = all_distinct_labels + all_labels

all_distinct_labels = list(set(all_distinct_labels))

for l in all_distinct_labels:
    property_tag_dict = get_pos_tags(l)
    # print('------------------')

print('number of all distinct labels', len(all_distinct_labels))
print('handled labels', len(handled_property))

tag_chain_set = set(unhandled_tag_chain)
counter_dict = {}
for t_c in unhandled_tag_chain:
    counter_dict[t_c] = unhandled_tag_chain.count(t_c)

sorted_dict = {k: v for k, v in sorted(counter_dict.items(), key=lambda item: item[1], reverse=True)}
print(sorted_dict)
