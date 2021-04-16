import os
import re

import tarfile
from pprint import pprint

from .JPS_query_constructor import JPS_query_constructor
from .jps_fallback_classifier import JPSQuestionClassifier
from .topic_classifier import TopicClassifier
#from rasa_nlu.model import Interpreter
# from .wiki_fallback_classifier import WikiQuestionTypeClassifier
import json
import sys
from SPARQLWrapper import SPARQLWrapper, JSON
import warnings


class Chatbot:

    def __init__(self, socketio):
        warnings.filterwarnings("ignore")
        # self.tc = TopicClassifier()
        self.jps_classifier = JPSQuestionClassifier()
        self.jps_query_constructor = JPS_query_constructor(socketio)

        # self.wiki_classifier = WikiQuestionTypeClassifier()

        # self.nlu_model_directory = '../rasa_default/models'
        # self.extract_nlu_model()  # extract trained model
        # self.interpreter = Interpreter.load('./nlu')  # load trained model
        # print('model loaded')

    # TODO: move the nlu models to the WikiClassifier

    # def extract_nlu_model(self):  # extract the newest trained nlu model
    #     path = self.nlu_model_directory
    #     files = os.listdir(path)
    #     paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
    #     file_name = max(paths, key=os.path.getctime)
    #     tf = tarfile.open(file_name)
    #     tf.extractall()
    '''
    def question_type_validation(self, result):
        # check whether the intent analyse result provides the necessary info
        intent = result['intent']['name']
    '''

    def simple_replace(self, question):
        # replace '==>' '==' '->' '+'
        question = re.sub(r'([-]>)|([=]+>)|([=]+)|([=]\])', 'equation_seperator', question)
        return question.replace('+', 'add_sign')

    def analyse_questions(self, question):
        question = self.simple_replace(question)
        # topics = self.tc.classify_topic(question)
        answer = 'failed'
        # mode = 'jps'
        # mode = 'wiki'
         # print('topics:', topics)
        # try:
        #     result = self.wiki_classifier.interpreter.parse(question)
        #     sparql_query = self.wiki_classifier.fill_Sparql(result)
        #     print('------------- intent result ----------------')
        #     pprint(result)
        #     print('------------- query constructed ------------')
        #     print(sparql_query)
        #     print('--------------query result -----------------')
        #     answer = self.wiki_classifier.fire_query(sparql_query)
        #     print(answer)
        #     print('result ---> ', json.loads(answer)['results']['bindings'])
        #     print('--------------------------------------------')
        #     if len(json.loads(answer)['results']['bindings']) == 0:
        #         raise Exception()
        # except:
        # RASA_JPS_MODELS_DIR
        result = self.jps_classifier.interpret(question)
        answer = self.jps_query_constructor.construct_query(result)
        pprint(answer)

        if type(answer) == tuple:
            answer = answer[0]


        if len(json.loads(answer)) == 0:
            return None

            # TODO: update the UI to show results returned from JPS endpoints
            # TODO: based on the result returned by the jps classifier, construct the query ...

        # TODO: Make sure that the typical OntoKin questions can be answered

        return answer
