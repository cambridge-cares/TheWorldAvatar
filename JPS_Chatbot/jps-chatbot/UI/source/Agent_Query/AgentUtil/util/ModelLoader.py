import os
from rasa.nlu.model import Interpreter
from .location import WIKI_MODELS_DIR, AGENT_MODELS_DIR, JPS_MODELS_DIR
import logging


# Specify the directory of the nlu models
class ModelLoader:
    def __init__(self):
        self.agent_nlu_model_directory = os.path.join(AGENT_MODELS_DIR, 'nlu')
        self.wiki_nlu_model_directory = os.path.join(WIKI_MODELS_DIR, 'nlu')
        self.jps_nlu_model_directory = os.path.join(JPS_MODELS_DIR, 'nlu')

    def AGENT_NLU_MODEL(self):
        return self.agent_nlu_model_directory
        # return Interpreter.load(self.agent_nlu_model_directory)

    def WIKI_NLU_MODEL(self):
        return Interpreter.load(self.wiki_nlu_model_directory)

    def JPS_NLU_MODEL(self):
        return Interpreter.load(self.jps_nlu_model_directory)
