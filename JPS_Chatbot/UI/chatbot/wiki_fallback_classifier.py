import json
import sys
import os
import re

import tarfile
from rasa.nlu.model import Interpreter
import json
import sys
import warnings
from SPARQLWrapper import SPARQLWrapper, JSON

from .wiki_search_interface import WiKiSearchInterface


class WikiQuestionTypeClassifier:

    def __init__(self):
        self.intent_types = ['item_attribute_query']
        with open('C:/Users/xz378_admin/PycharmProjects/JPS_Chemistry_Chatbot/SPARQL_template.json') as f:
            self.templates = json.loads(f.read())
        self.serach_interface = WiKiSearchInterface()
        warnings.filterwarnings("ignore")
        self.nlu_model_directory = '../rasa_default/models'
        self.extract_nlu_model()  # extract trained model
        self.interpreter = Interpreter.load('./nlu')  # load trained model
        print('model loaded')

    # TODO: move the nlu models to the WikiClassifier
    def extract_nlu_model(self):  # extract the newest trained nlu model
        path = self.nlu_model_directory
        files = os.listdir(path)
        paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
        file_name = max(paths, key=os.path.getctime)
        tf = tarfile.open(file_name)
        tf.extractall()

    def fill_Sparql(self, result):
        sparql_query = ''
        entities = result['entities']
        intent_type = result['intent']['name']
        print('intent type', intent_type)
        sparql_template = self.templates[intent_type]
        print(sparql_template)
        if intent_type == 'simple_info':
            for entity in entities:
                if entity['entity'] == 'entity':
                    target = entity['value']
                    target = self.serach_interface.get_first_match(target)
                    sparql_query = sparql_template % (target)

        elif intent_type == 'item_attribute_query':
            target = ''
            attribute = ''
            for entity in entities:
                if entity['entity'] == 'entity':
                    target = entity['value']
                    target = self.serach_interface.get_first_match(target)

                if entity['entity'] == 'attribute':
                    attribute = entity['value']
                    attribute = self.serach_interface.get_first_match(attribute)
            sparql_query = sparql_template % (target, attribute)

        elif intent_type == 'relation_filter':
            target = ''
            attribute = ''
            _class = ''
            for entity in entities:
                if entity['entity'] == 'entity':
                    target = entity['value']
                    target = self.serach_interface.get_first_match(target)

                if entity['entity'] == 'attribute':
                    attribute = entity['value']
                    attribute = self.serach_interface.get_first_match(attribute)

                if entity['entity'] == 'class':
                    _class = entity['value']
                    _class = self.serach_interface.get_first_match(_class)
            sparql_query = sparql_template % (_class, attribute, target)

        elif intent_type == 'batch_restriction_query':
            attribute = ''
            _class = ''
            _entity = ''
            for entity in entities:

                if entity['entity'] == 'attribute':
                    attribute = entity['value']
                    attribute = self.serach_interface.get_first_match(attribute)

                if entity['entity'] == 'class':
                    _class = entity['value']
                    _class = self.serach_interface.get_first_match(_class)

                if entity['entity'] == 'entity':
                    _entity = entity['value']
                    _entity = self.serach_interface.get_first_match(_entity)

            if _class == '':
                _class = _entity

            sparql_query = sparql_template % (_class, attribute,attribute,attribute)

        elif intent_type == 'batch_restriction_numerical_query':
            attribute = ''
            _class = ''
            number = ''
            operator = ''

            more = ['larger', 'more']
            less = ['less', 'smaller']
            for entity in entities:

                if entity['entity'] == 'comparison':
                    comparison = entity['value']
                    print(comparison)
                    if comparison in more:
                        operator = '>'
                    else:
                        operator = '<'

                if entity['entity'] == 'number':
                    number = entity['value']

                if entity['entity'] == 'attribute':
                    attribute = entity['value']
                    attribute = self.serach_interface.get_first_match(attribute)

                if entity['entity'] == 'class':
                    _class = entity['value']
                    _class = self.serach_interface.get_first_match(_class)
            sparql_query = sparql_template % (_class, attribute, operator, number)

        elif intent_type == 'item_attribute_query_with_condition':
            # TODO: this is a difficult one, lets figure how does qualifier work in wikidata first.
            pass

        return sparql_query

    def fire_query(self, query):
        endpoint_url = "https://query.wikidata.org/sparql"
        user_agent = "WDQS-example Python/%s.%s" % (sys.version_info[0], sys.version_info[1])
        sparql = SPARQLWrapper(endpoint_url, agent=user_agent)
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return json.dumps(results)
