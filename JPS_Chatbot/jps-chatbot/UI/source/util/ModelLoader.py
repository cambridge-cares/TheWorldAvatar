import os
from rasa.nlu.model import Interpreter
from .location import WIKI_MODELS_DIR, AGENT_MODELS_DIR, JPS_MODELS_DIR
import logging

# Specify the directory of the nlu models

agent_nlu_model_directory = os.path.join(AGENT_MODELS_DIR, 'nlu')
wiki_nlu_model_directory = os.path.join(WIKI_MODELS_DIR, 'nlu')
jps_nlu_model_directory = os.path.join(JPS_MODELS_DIR, 'nlu')

# print(jps_nlu_model_directory)

AGENT_NLU_MODEL = Interpreter.load(agent_nlu_model_directory)
logging.info('Agent NLU model loaded')
WIKI_NLU_MODEL = Interpreter.load(wiki_nlu_model_directory)
logging.info('Wiki NLU model loaded')
JPS_NLU_MODEL = Interpreter.load(jps_nlu_model_directory)
logging.info('JPS NLU model loaded')




