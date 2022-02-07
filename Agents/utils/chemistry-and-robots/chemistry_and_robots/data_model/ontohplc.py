import pydantic
from typing import List

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontolab import *

class HPLCReport(BaseOntology):
    clz: str = ONTOHPLC_HPLCREPORT
