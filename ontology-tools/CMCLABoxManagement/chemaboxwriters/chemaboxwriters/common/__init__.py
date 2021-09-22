from chemaboxwriters.common.handlers import QC_LOG_TO_QC_JSON, CSV_TO_OWL
from chemaboxwriters.common.base import StageHandler, Pipeline
from chemaboxwriters.common.stageenums import aboxStages
import json
import os
import pkg_resources

PREFIX_FILE = pkg_resources.resource_filename(__name__,'prefixes.json')

with open(PREFIX_FILE, 'r') as pfile:
    PREFIXES = json.load(pfile)