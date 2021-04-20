from pprint import pprint
from rasa.nlu.model import Interpreter
import os, sys
import tempfile
import tarfile
import nltk

# sys.path.append(".")

entity_intent_map = {'item_attribute_query': {'attribute': None, 'entity': None},
                     'batch_restriction_query': {'attribute': None, 'class': None},
                     'batch_restriction_query_numerical': {'class': None, 'attribute': None,
                                                           'comparison': None, 'numerical_value': None},
                     'batch_restriction_query_numerical_and_attribute': {'attribute': [],
                                                                         'class': None, 'comparison': None,
                                                                         'numerical_value': None}
                     }


def extract_nlu_model(extract_dir='../models'):
    # Identify the newest trained nlu model
    path = '../models/'
    files = os.listdir(path)
    paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
    file_name = max(paths, key=os.path.getctime)
    # Extract the model to a temporary directory
    tf = tarfile.open(file_name)
    tf.extractall(path=extract_dir)


def parse_question_interpretation(question):
    result = interpreter.parse(question)
    pprint(result)
    # get the key components and their types out
    # get the intent of the question
    intent = result['intent']['name']
    entities = result['entities']
    # item_attribute_query   :                          attribute + entity
    # batch_restriction_query:                          attribute + class
    # batch_restriction_query_numerical :               class + attribute + comparison + numerical_value
    # batch_restriction_query_numerical_and_attribute:  attribute + class + attribute + comparison + numerical_value
    # print('intent:', intent, '\nentities', entities)
    result = fill_in_components(intent, entities)
    return result

def fill_in_components(intent, entities):
    obj = entity_intent_map[intent]
    for entity in entities:
        entity_type = entity['entity']
        term = entity['value'].lower()
        if term.strip().lower() in stopwords and not (entity_type == 'comparison'):
            pass
        else:
            slot = obj[entity_type]
            if type(slot) is list:
                obj[entity_type].append(term)
                # more than one term should present ...
            else:
                obj[entity_type] = term

    return {'type': intent, 'entities': obj}


# extract_nlu_model()
stopwords = nltk.corpus.stopwords.words('english')
stopwords.append('all')
interpreter = Interpreter.load('../models/nlu')

 
gold_question = 'what is the pka of all the acids with a molecular weight over 200'
silver_question = 'show me the heat capacity of all the Amino Acid'
bronze_question = 'what is the boling point of ch4'

# get the gold_question
# print('============ the gold result ===========')
# parse_question_interpretation(gold_question)
print('============ the silver result =========')
r1 = parse_question_interpretation(silver_question)
pprint(r1)
print('============ the bronze result =========')
r2 = parse_question_interpretation(bronze_question)
pprint(r2)

