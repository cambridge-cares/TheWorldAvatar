import json

# need one property label , one instance label
import math
import random
import re
from typing import TextIO

# from pattern.en import pluralize, singularize
import nltk
from pattern.text import singularize

from util.property_processor import *


def pluralize_class(c):
    return c
    # if c in PLURALIZE_DICT:
    #     pc = PLURALIZE_DICT[c]
    # else:
    #     pc = pluralize(c)
    #     PLURALIZE_DICT[c] = pc
    # return pc


def process_instance_brackets(i):
    return i.replace('[', '').replace(']', '').replace('(', '').replace(')', '')


# on top of a batch attribute query, rank the results
def rank_query(classes, quantity_properties):
    # sort aromatic hydrocarbons by their weight
    # rank xxx by their
    _questions = []
    dts = ['their', 'the', '']
    # ranks = ['rank', 'sort', 'list', 'show']
    ranks = ['rank', 'sort']
    for c in classes:
        c = process_instance_brackets(c)
        pc = pluralize_class(c)
        # c = random.choice([c, pc])
        for p in quantity_properties:
            p = p.strip()
            q = '%s %s [%s](class) by %s [%s](attribute)' % (
                random.choice(ranks), random.choice(ALLs), c, random.choice(dts), p)
            _questions.append(q)
    return _questions


def attribute_batch_attribute_query_numerical(classes, quantity_properties, properties):
    # what are the geometry of hydrocarbons with mass over 15
    _questions = []
    for c in classes:
        c = process_instance_brackets(c)
        pc = pluralize_class(c)
        # c = random.choice([c, pc])
        c = '[%s](class)' % c
        for p in quantity_properties:
            number = round(random.uniform(-100, 100), random.choice([0, 1, 2]))
            comparison = random.choice(smaller_than + larger_than)
            head = random.choice(heads)
            connect = random.choice(connects)
            _all = random.choice(ALLs)
            for attribute in random.sample(properties, 2):
                # q = 'head%s all%s [c%s](class) connects%s p%s more than/ less than number%s'
                q = '%s [%s](attribute) %s %s %s [%s](attribute) [%s](comparison) [%s](number)' % (head, attribute.strip(), _all, c, connect, p.strip(), comparison, number)
                _questions.append(q)

    return _questions

# query more than one attribute at a time
def item_multi_attribute_query(instances, properties):
    # show both the geometry and molecular weight of benzene
    # geometry, molecular weight, and density of benzene
    _questions = []
    for i in instances:
        # randomly generate pairs, triples (2 - 5) properties pairs
        i = '[%s](species)' % process_instance_brackets(i)
        n = random.choice([2, 3, 4, 5])
        n = min(n, len(properties))
        random_attributes = random.sample(properties, n)
        attribute_chain = '[%s](attribute)' % random_attributes[0].strip()
        for p in random_attributes[1:]:
            AND = random.choice([', ', ',and', ' and'])
            p = p.strip()
            p = '[%s](attribute)' % p
            attribute_chain = (AND + ' ').join([attribute_chain, p])
        q = '%s %s %s %s %s' % (
            random.choice(heads), random.choice(['the', '']), attribute_chain, random.choice(['of', '']), i)
        q2 = '%s %s quote %s' % (random.choice(heads), i, attribute_chain)
        _questions = _questions + [q, q2]

    return _questions


# general abstract
def about_query(instances):
    _questions = []
    for i in instances:
        # _heads = ['what is', 'describe', 'introduce']
        _heads = ['']
        h = random.choice(_heads)
        q = '%s [%s](species)' % (h, i)
        _questions.append(q)
    return _questions


def batch_query(classes):
    _questions = []
    # list all hydrocarbons
    for c in classes:
        c = process_instance_brackets(c)
        pc = pluralize_class(c)
        # c = random.choice([c, pc])
        q = '%s %s [%s](class)' % (random.choice(heads), random.choice(ALLs), c)
        _questions.append(q)
    return _questions


def item_attribute_query(ID, instances, properties):
    _questions = []
    for i in instances:
        i = process_instance_brackets(i)
        i = '[%s](species)' % i
        for p in properties:
            if ' ' not in p.strip():  # the attribute has only one word
                p = random.choice([p, singularize(p.replace(' of ', ' ')), pluralize_class(p.replace(' of ', ' '))
                                   + ' of'])
            rst = get_pos_tags(p)
            if rst:
                tag_chain = rst[0]
                property_tags_dict = rst[1]
                creation_result = generate_simple_questions(tag_chain, property_tags_dict, i, p)
                _questions = _questions + creation_result[0]
                FULL_PROPERTY_TAG_DICT.update(creation_result[1])
    return _questions


def batch_attribute_query(ID, classes, properties):
    _questions = []
    for c in classes:
        c = process_instance_brackets(c)
        pc = pluralize_class(c)
        # c = random.choice([c, pc])
        c = '[%s](class)' % c
        for p in properties:
            # if ' ' not in p.strip():  # the attribute has only one word
            #     p = random.choice([p, pluralize_class(p.replace(' of', '')), pluralize_class(p.replace(' of', ' '))
            #                        + 'of'])
            # chemical structure -> chemical structures
            # else:
            #     last_bit = p.split(' ')[-1]
            #     p_last_bit = pluralize_class(last_bit)
            #     p = p.replace(' ' + last_bit, ' ' + p_last_bit)

            rst = get_pos_tags(p)
            if rst:
                tag_chain = rst[0]
                property_tags_dict = rst[1]
                creation_result = generate_simple_questions(tag_chain, property_tags_dict, c, p)
                _questions = _questions + creation_result[0]
                FULL_PROPERTY_TAG_DICT.update(creation_result[1])
    return _questions


def batch_attribute_query_numerical(classes, properties):
    # show all aromatic hydrocarbons with weight more than 123
    _questions = []
    for c in classes:
        c = process_instance_brackets(c)
        pc = pluralize_class(c)
        # c = random.choice([c, pc])
        c = '[%s](class)' % c
        for p in properties:
            number = round(random.uniform(-100, 100), random.choice([0, 1, 2]))
            comparison = random.choice(smaller_than + larger_than)
            head = random.choice(heads)
            connect = random.choice(connects)
            _all = random.choice(ALLs)
            # q = 'head%s all%s [c%s](class) connects%s p%s more than/ less than number%s'
            q = '%s %s %s %s [%s](attribute) [%s](comparison) [%s](number)' % (
                head, _all, c, connect, p.strip(), comparison, number)
            _questions.append(q)

    return _questions


def get_instance_labels(ID):
    tmp = []
    with open('C:/data/instance_info/%s' % ID) as f:
        data = json.loads(f.read())['results']['bindings']
        for b in data:
            if 'label' in b:
                label = b['label']['value'].replace('[', '').strip()
            tmp.append(label.strip())
            if 'alt_label' in b:
                alt_label = [al.strip().replace('[', '') for al in b['altLabel_list']['value'].split('$')]
                tmp = tmp + alt_label
            if 'formula' in b:
                formula = b['formula']['value']
                subscript = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789")
                formula = formula.translate(subscript)
                tmp.append(formula.strip().replace('[', ''))
    ID = 'http://www.wikidata.org/entity/' + ID
    if ID in URI_SMILES_DICT:
        SMILES = URI_SMILES_DICT[ID]
        tmp.append(SMILES)
    return set([t.replace('(', '').replace(')', '') for t in tmp])


def get_class_labels():
    class_labels = []
    with open('C:/data/instance_info/%s' % ID) as f:
        data = json.loads(f.read())['results']['bindings']
        for b in data:
            if 'classLabel' in b:
                l = b['classLabel']['value']
                if l.lower() not in STOP_CLASSES:
                    class_labels.append(l)
    return set(class_labels)


def get_property_labels(ID):
    stop_properties = ['electric dipole moment','dipole moment', 'formal charge', 'electronic energy',
                       'lennard jones well depth', 'polarizability', 'rotational relaxation']

    property_labels = []
    quantity_property_labels = []
    properties = INSTANCE_PROPERTY_DICT[ID]['properties']
    for p in properties:
        _type = p['type']
        if 'ExternalId' in _type:
            pass
        else:
            label = p['label']
            for i in range(5):
                property_labels.append(label)
            uri = p['uri']
            uri = uri.replace('http://www.wikidata.org/entity/', '')
            if uri in EXTRA_PROPERTIES:
                for i in range(5):
                    property_labels = property_labels + EXTRA_PROPERTIES[uri]
            if 'alt_label' in p:
                alt_label = p['alt_label']
                property_labels = property_labels + alt_label
            if 'Quantity' in _type:
                if 'alt_label' in p:
                    alt_label = p['alt_label']
                    quantity_property_labels = quantity_property_labels + alt_label
                for i in range(5):
                    quantity_property_labels.append(label)
                    if uri in EXTRA_PROPERTIES:
                        quantity_property_labels = quantity_property_labels + EXTRA_PROPERTIES[uri]

    property_labels = [l for l in property_labels if l.lower() not in stop_properties]
    quantity_property_labels = [l for l in quantity_property_labels if l.lower() not in stop_properties]
    return property_labels, quantity_property_labels


with open('../../files/FULL_PROPERTY_TAG_DICT') as f:
    FULL_PROPERTY_TAG_DICT = json.loads(f.read())

with open('../../files/instance_property_mapping_first_2000') as f:
    instance_property_mapping_first_2000 = json.loads(f.read())

with open('../../files/instance_property_mapping_random_3000') as f:
    instance_property_mapping_random_3000 = json.loads(f.read())

with open('../../files/EXTRA_PROPERTIES') as f:
    EXTRA_PROPERTIES = json.loads(f.read())

with open('../../files/URI_SMILES_DICT') as f:
    URI_SMILES_DICT = json.loads(f.read())

with open('../../files/PLURALIZE_DICT') as f:
    PLURALIZE_DICT = json.loads(f.read())

INSTANCE_PROPERTY_DICT = {}
INSTANCE_PROPERTY_DICT.update(instance_property_mapping_first_2000)
INSTANCE_PROPERTY_DICT.update(instance_property_mapping_random_3000)
STOP_CLASSES = ['object', 'perceptible object', 'metaclass',
                'spatio-temporal entity', 'class', 'part', 'taxon',
                'component', 'abstract object', 'temporal entity', 'entity',
                'concrete object', 'contradiction', 'occurrence', 'uncertainty',
                'spatial entity', 'state', 'property', 'occurrent', 'use',
                'therapeutic use', 'means', 'activity', 'physical object',
                'collection entity', 'quality', 'human activity', 'series',
                'product', 'common matter', 'group or class of chemical substances',
                'goods and services', 'pharmacologic action', 'set', 'result',
                'intentional human action', 'action', 'chemical actions and uses',
                'human behaviour', 'mode of action', 'mode of toxic action',
                'group', 'process', 'planned processes', 'inconsistency', 'phenomenon',
                'health risks', 'operational risk', 'risk', 'risk source',
                'change', 'capability', 'group or class of physical objects', 'symbols'
                ]

# there are several types of properties
# 1. WikibaseItem -> a URI
# 2. CommonsMedia -> a picture
# 3. ExternalId -> CAS/EC etc.
# 4. String -> a String
# 5. Quantity -> a number,  we like this one
#############################################
# The current filtering strategy:
# filter out most of the ExternalId properties

# these lists hold questions of different types
item_attribute_query_list = []
batch_attribute_query_list = []
batch_attribute_query_numerical_list = []
attribute_batch_attribute_query_numerical_list = []
rank_query_list = []
batch_query_list = []
about_query_list = []
item_multi_attribute_query_list = []
counter = 0
factor = 5
total = len(INSTANCE_PROPERTY_DICT.keys())
RUN_SAMPLE = list(INSTANCE_PROPERTY_DICT.keys())
# test
# RUN_SAMPLE = random.sample(RUN_SAMPLE, 100)
for ID in RUN_SAMPLE:
    # print(ID)
    counter = counter + 1
    print(counter, 'out of', total)
    CLASS_LABELS = get_class_labels()
    property_rst = get_property_labels(ID)
    PROPERTY_LABELS = property_rst[0]
    QUANTITY_PROPERTY_LABELS = property_rst[1]
    INSTANCE_LABELS = get_instance_labels(ID)
    item_attribute_query_list = item_attribute_query_list + item_attribute_query(ID, INSTANCE_LABELS, PROPERTY_LABELS)
    batch_attribute_query_list = batch_attribute_query_list + batch_attribute_query(ID, CLASS_LABELS, PROPERTY_LABELS)
    batch_attribute_query_numerical_list = batch_attribute_query_numerical_list \
                                           + batch_attribute_query_numerical(CLASS_LABELS, QUANTITY_PROPERTY_LABELS)
    rank_query_list = rank_query_list + rank_query(CLASS_LABELS, QUANTITY_PROPERTY_LABELS)
    batch_query_list = batch_query_list + batch_query(CLASS_LABELS)
    about_query_list = about_query_list + about_query(INSTANCE_LABELS)
    item_multi_attribute_query_list = item_multi_attribute_query_list + item_multi_attribute_query(INSTANCE_LABELS,
                                                                                                   PROPERTY_LABELS)
    attribute_batch_attribute_query_numerical_list = attribute_batch_attribute_query_numerical_list + attribute_batch_attribute_query_numerical(CLASS_LABELS, QUANTITY_PROPERTY_LABELS, PROPERTY_LABELS)

# ====================================== generate the blocks in the nlu.md file ==============================

# other_whs = ['who', 'where', 'when', 'what', 'at what time', 'at what place', '']
other_whs = ['']
questions = random.sample(item_attribute_query_list, math.ceil(len(item_attribute_query_list) / 200 * factor))
item_attribute_block = '\n ## intent:%s\n' % 'item_attribute_query' + '\n - ' + '\n - '.join(questions)
for i in range(10):
    item_attribute_block = item_attribute_block.replace('what ', random.choice(other_whs) + ' ', 1)

with open('test/data/item_attribute_block', 'w', encoding='utf-8') as f:
    f.write(item_attribute_block)

print('item_attribute_query', len(questions))

questions = random.sample(batch_attribute_query_list, math.ceil(len(batch_attribute_query_list) / 800 * factor))
batch_attribute_block = '\n ## intent:%s\n' % 'batch_attribute_query' + '\n - ' + '\n - '.join(questions)

print('batch_attribute_query', len(questions))

# .replace(
  #  ' is ', ' are ')
for i in range(10):
    batch_attribute_block = batch_attribute_block.replace('what ', random.choice(other_whs) + ' ', 1)

with open('test/data/batch_attribute_block', 'w', encoding='utf-8') as f:
    f.write(batch_attribute_block)

questions = random.sample(batch_attribute_query_numerical_list,
                          math.ceil(len(batch_attribute_query_numerical_list) / 100 * factor))
batch_attribute_query_numerical_block = '\n ## intent:%s\n' % 'batch_attribute_query_numerical' + '\n - ' + \
                                        '\n - '.join(questions)
for i in range(10):
    batch_attribute_query_numerical_block = batch_attribute_query_numerical_block.replace('what ',
                                                                                      random.choice(other_whs) + ' ', 1)
print('batch_attribute_query_numerical', len(questions))


with open('test/data/batch_attribute_query_numerical_block', 'w', encoding='utf-8') as f:
    f.write(batch_attribute_query_numerical_block)

questions = random.sample(rank_query_list, math.ceil(len(rank_query_list) / 100 * factor))
rank_query_list_block = '\n ## intent:%s\n' % 'rank_query_list' + '\n - ' + '\n - '.join(questions)
for i in range(10):
    rank_query_list_block = rank_query_list_block.replace('what ', random.choice(other_whs) + ' ', 1)

print('rank_query_list_block', len(questions))

with open('test/data/rank_query_list_block', 'w', encoding='utf-8') as f:
    f.write(rank_query_list_block)

questions = random.sample(batch_query_list, min(math.ceil(len(batch_query_list) / 2.5 * factor), len(batch_query_list)))
batch_query_block = '\n ## intent:%s\n' % 'batch_query' + '\n - ' + '\n - '.join(questions)
for i in range(10):
    batch_query_block = batch_query_block.replace('what ', random.choice(other_whs) + ' ', 1)
print('batch_query_block', len(questions))


questions = random.sample(attribute_batch_attribute_query_numerical_list, math.ceil(len(attribute_batch_attribute_query_numerical_list)/ 200 * factor))
attribute_batch_attribute_query_numerical_block = '\n ## intent:%s\n' % 'attribute_batch_attribute_query_numerical' + '\n - ' + '\n - '.join(questions)
for i in range(10):
    attribute_batch_attribute_query_numerical_block = attribute_batch_attribute_query_numerical_block.replace('what ', random.choice(other_whs) + ' ', 1)

print('attribute_batch_attribute_query_numerical', len(questions))

with open('test/data/batch_query_block', 'w', encoding='utf-8') as f:
    f.write(batch_query_block)

sample_number = min(len(about_query_list) / 1 * factor, len(about_query_list))
questions = random.sample(about_query_list, math.ceil(sample_number))
about_query_block = '\n ## intent:%s\n' % 'about_query' + '\n - ' + '\n - '.join(questions)
for i in range(10):
    about_query_block = about_query_block.replace('what ', random.choice(other_whs) + ' ', 1)

print('about_query_block', len(questions))


# questions = random.sample(item_multi_attribute_query_list,
#                           math.ceil(len(item_multi_attribute_query_list) / 20 * factor))
# item_multi_attribute_query_block = '\n ## intent:%s\n' % 'item_multi_attribute_query' + '\n - ' + '\n - '.join(
#     questions)
# for i in range(10):
#     item_multi_attribute_query_block = item_multi_attribute_query_block.replace('what ', random.choice(other_whs) + ' ', 1)

# ========================================================================================================

block = item_attribute_block + '\n' + batch_attribute_block + '\n' + batch_attribute_query_numerical_block + \
        '\n' + rank_query_list_block + '\n' + batch_query_block + '\n' + about_query_block + '\n' + attribute_batch_attribute_query_numerical_block
        # + item_multi_attribute_query_block

_RE_COMBINE_WHITESPACE = re.compile(r"[ ]+")
block = _RE_COMBINE_WHITESPACE.sub(" ", block).strip()
block = block.replace('[[', '[').replace(']]', ']').lower().replace('what ', '').replace(' are ', ' is ').replace(' is ', '')

print(len(questions))
print(len(item_attribute_query_list))

import datetime

dt = datetime.datetime.today()
month = dt.month
day = dt.day

with open('test/data/nlu_0%s_0%s_factor_%s.md' % (month, day, factor), 'w', encoding='utf-8') as f:
    f.write(block)

with open('../../files/FULL_PROPERTY_TAG_DICT', 'w') as f:
    f.write(json.dumps(FULL_PROPERTY_TAG_DICT))

with open('../../files/PLURALIZE_DICT', 'w') as f:
    f.write(json.dumps(PLURALIZE_DICT))

# SMILES
# FORMULA
# NAMES
# PROPERTIES
# CLASS
