import json
from pprint import pprint
from rasa.nlu.model import Interpreter
import os, sys
import tempfile
import tarfile
import nltk


# try:
#     from __main__ import socketio
#
#     print('Importing socketIO from main in interpretation')
# except ImportError:
#     from run import socketio
#
#     print('Importing socketIO from run_socket in interpretation')

class InterpretationParser:

    def __init__(self, socketio):
        self.interpreter = None
        self.entity_intent_map = {'item_attribute_query': {'attribute': None, 'entity': None},
                                  'batch_restriction_query': {'attribute': None, 'class': None},
                                  'batch_restriction_query_numerical': {'class': None, 'attribute': None,
                                                                        'comparison': None, 'numerical_value': None},
                                  'batch_restriction_query_numerical_and_attribute': {'attribute': [],
                                                                                      'class': None, 'comparison': None,
                                                                                      'numerical_value': None}
                                  }
        self.socketio = socketio

    def parse_question_interpretation(self, question):
        result = self.interpreter.parse(question)
        pprint(result)
        # get the key components and their types out
        # get the intent of the question
        intent = result['intent']['name']
        entities = result['entities']
        result = self.fill_in_components(intent, entities)
        return result

    def fill_in_components(self, intent, entities):
        obj = self.entity_intent_map[intent]
        for entity in entities:
            entity_type = entity['entity']
            term = entity['value'].lower()

            slot = obj[entity_type]
            if type(slot) is list:
                obj[entity_type].append(term)
                # more than one term should present ...
            else:
                obj[entity_type] = term
        print('============ the interpretation results ==========')
        pprint(obj)
        self.socketio.emit('coordinate_agent',
                           'The interpretation result from WIKI' + json.dumps(obj).replace('{', '').replace('}', ''))
        return {'type': intent, 'entities': obj}
