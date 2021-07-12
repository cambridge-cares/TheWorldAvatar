import os
import re

import tarfile
from pprint import pprint

from .JPSQueryConstructor import JPSQueryConstructor
from .jps_fallback_classifier import JPSQuestionClassifier
from .topic_classifier import TopicClassifier
# from rasa_nlu.model import Interpreter
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
        self.jps_query_constructor = JPSQueryConstructor(socketio)

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
        result = self.jps_classifier.interpret(question)
        answer = self.jps_query_constructor.construct_query(result)
        if type(answer) == tuple:
            answer = answer[0]

        if len(json.loads(answer)) == 0:
            return None

            # TODO: update the UI to show results returned from JPS endpoints
            # TODO: based on the result returned by the jps classifier, construct the query ...

        # TODO: Make sure that the typical OntoKin questions can be answered

        return answer
