import json
import sys, os

from Chatbot.Interpretation_parser import InterpretationParser
from Chatbot.SearchEngine import SearchEngine
from Chatbot.SPARQLConstructor import SPARQLConstructor
from Chatbot.SPARQLQuery import SPARQLQuery
from Chatbot.LDA_classifier import LDAClassifier
from rasa_jps.chatbot_interface import Chatbot
from functools import lru_cache

from pprint import pprint
from rasa.nlu.model import Interpreter
import os
import tarfile

from location import WIKI_MODELS_DIR


# 0. get the topic model result, choose which direction it goes
# 1. get the InterpretationParse

def extract_nlu_model(extract_dir='../models/'):
    # Identify the newest trained nlu model
    # Disable the function when deployed to production server ... 
    path = 'models/'
    files = os.listdir(path)
    print('the directory of files', files)
    paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
    file_name = max(paths, key=os.path.getctime)
    # Extract the model to a temporary directory
    tf = tarfile.open(file_name)
    tf.extractall(path=extract_dir)


class CoordinateAgent:
    def __init__(self, socketio):
        # initialize interpreter
        # extract_nlu_model()
        self.stopwords = ['all', 'the']
        # self.stopwords.append('all')

        self.nlu_model_directory = os.path.join(WIKI_MODELS_DIR, 'nlu')
        self.interpreter = Interpreter.load(self.nlu_model_directory)  # load the wiki nlu models
        self.jps_interface = Chatbot(socketio)
        # try:
        #     from __main__ import socketio
        #     print('Importing socketIO from main')
        # except ImportError:
        #     from run import socketio
        #     print('Importing socketIO from run_socket')
        self.socket = socketio

    # TODO: separate the function of topic identifying
    # TODO: separate the SPARQL query
    # TODO: show progress
    # def identify_topics(self, question):

    def question_classification(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        return intent_and_entities['intent']

    def named_entity_recognition(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        return intent_and_entities['entities']

    @lru_cache(maxsize=None)
    def run(self, question):

        # TODO: put the LDA model here
        # ===================== initialize the things for wiki
        self.interpreter_parser = InterpretationParser(self.socket)
        self.interpreter_parser.interpreter = self.interpreter
        print('Loading interpreter')
        self.search_engine = SearchEngine()
        self.sparql_constructor = SPARQLConstructor()
        self.sparql_query = SPARQLQuery(self.socket)

        self.lda_classifier = LDAClassifier()
        topics = self.lda_classifier.classify(question)
        print('============== topics ==============')
        print(topics)
        if topics == 'ERROR002':
            self.socket.emit('coordinate_agent', 'This question seems to be outside the chemistry domain')
        else:
            self.socket.emit('coordinate_agent', 'The topics identified are: ' + json.dumps(topics))

        for topic in topics:
            if topic == 'wiki':
                try:
                    result = self.wiki_query(question)
                    if result is None:
                        pass
                    else:
                        return result
                except:
                    pass

            else:
                try:
                    result = self.jps_interface.analyse_questions(question)
                    print('----- the result by jps ----')
                    if result is None:
                        pass
                    else:
                        return result
                except:
                    pass
        return 'Nothing'
        # result = self.wiki_query(question)
        # return result
        # # for topic in topics:
        # if topic == 'wiki':
        #     try:
        #         result = self.wiki_query(question)
        #         return result
        #     except:
        #         # TODO: add JPS results to it
        #         pass

    @lru_cache(maxsize=64)
    def wiki_query(self, question):
        intent_and_entities = self.interpreter_parser.parse_question_interpretation(question)
        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        if intent_and_entities_with_uris is None:
            return None
        elif intent_and_entities_with_uris == 'Error001':
            # now switch intent to item_attribute_query, and the entity to entity...
            print(intent_and_entities)
            print('we have a Error001')
            intent_and_entities['type'] = 'item_attribute_query'
            intent_and_entities['entities']['entity'] = intent_and_entities['entities']['class']

        print('before it breaks 001', intent_and_entities)
        intent_and_entities_with_uris = self.search_engine.parse_entities(intent_and_entities)
        print('================= result with uris ================')
        pprint(intent_and_entities_with_uris)
        sparqls = self.sparql_constructor.fill_sparql_query(intent_and_entities_with_uris)
        if sparqls is None:
            print('No valid SPARQL is returned')
            return None
        if len(sparqls) >= 5:
            sparqls = sparqls[:5]

        result = self.sparql_query.start_queries(sparqls)
        print('-------------- we have a result -------------------')
        # pprint(result)
        print('the result in json format', result[0])
        print('the sparql query', result[2])
        return result[0]

# ca = CoordinateAgent()
# ca.run('what reactions produce NO2 + O2')
# ca.run('find all the fatty acids with molecular weight more than 100')
# ca.run('the kindling point of C2HBrClF3')
# # # r = ca.run(question='what is the molecular weight of benzene')
# # ca.run(question='show me the heat capacity of glucose')
# # ca.run(question='what is the chemical structure of glucose')
# ca.run('show me the boliing point of ch4')
