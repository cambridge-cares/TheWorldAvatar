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

    def simple_replace(self, question):
        # replace '==>' '==' '->' '+'
        if 'CH2=CHCHO'.lower() not in question.lower():
            question = re.sub(r'([-]>)|([=]+>)|([=]+)|([=]\])', 'equation_seperator', question)
        return question.replace('+', 'add_sign')

    def analyse_questions(self, question):
        question = self.simple_replace(question)
        result = self.jps_classifier.interpret(question)
        try:
            answer = self.jps_query_constructor.construct_query(result)
        except:
            print('JPS failed to answer the question')
            return None
        if type(answer) == tuple:
            answer = answer[0]

        if len(json.loads(answer)) == 0:
            return None
        return answer
