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
from .search_interface import SearchInterface

class JPSQuestionClassifier:

    def __init__(self):
        with open('C:/Users/xz378_admin/PycharmProjects/JPS_Chemistry_Chatbot/JPS_SPARQL_template.json') as f:
            self.templates = json.loads(f.read())
        # TODO: make the template for the SPARQL query ...
        self.serach_interface = SearchInterface()
        warnings.filterwarnings("ignore")
        self.nlu_model_directory = '../rasa_jps/models'
        self.extract_nlu_model()  # extract trained model
        self.interpreter = Interpreter.load('./nlu')  # load trained model
        print('model loaded')

    def extract_nlu_model(self):  # extract the newest trained nlu model
        path = self.nlu_model_directory
        files = os.listdir(path)
        paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
        file_name = max(paths, key=os.path.getctime)
        tf = tarfile.open(file_name)
        tf.extractall()

    def interpret(self, question):
        return self.interpreter.parse(question)