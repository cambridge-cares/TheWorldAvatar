from chemaboxwriters.common.handlers import QC_LOG_TO_QC_JSON, CSV_TO_OWL
from chemaboxwriters.common.base import StageHandler, Pipeline
from chemaboxwriters.common.stageenums import aboxStages
import json
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
PREFIX_FILE = os.path.join(THIS_DIR,'prefixes.json')

with open(PREFIX_FILE, 'r') as pfile:
    PREFIXES = json.load(pfile)