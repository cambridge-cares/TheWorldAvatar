import json
import sys
import os
import re
import tempfile

import tarfile
from rasa.nlu.model import Interpreter
import json
import sys
import warnings
from SPARQLWrapper import SPARQLWrapper, JSON

from .locations import JPS_SPARQL_TEMPLATE_PATH, RASA_JPS_MODELS_DIR
from .search_interface import SearchInterface


class JPSQuestionClassifier:

    def __init__(self):
        # TODO: make the template for the SPARQL query ...
        self.serach_interface = SearchInterface()
        warnings.filterwarnings("ignore")

        self.nlu_model_directory = os.path.join(RASA_JPS_MODELS_DIR, 'nlu')

        # Extract the trained model to a temporary directory where we are guaranteed to have write access
        # temp_dir_path = tempfile.TemporaryDirectory().name
        # self.extract_nlu_model(temp_dir_path)
        # # Load the trained model from the temporary directory
        # model_path = os.path.join(temp_dir_path, "nlu")
        self.interpreter = Interpreter.load(self.nlu_model_directory)

    def extract_nlu_model(self, extract_dir):
        # Identify the newest trained nlu model
        path = self.nlu_model_directory
        files = os.listdir(path)
        paths = [os.path.join(path, basename) for basename in files if ('.tar' in basename)]
        file_name = max(paths, key=os.path.getctime)
        # Extract the model to a temporary directory
        tf = tarfile.open(file_name)
        tf.extractall(path=extract_dir)

    def interpret(self, question):
        return self.interpreter.parse(question)
