import os
import sys

from twa.kg_operations          import PySparqlClient

PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
from rework_ontomops.update_kg  import config_a_box_updates

a_box_updates_config        = config_a_box_updates("../OntoMOPConnection.env")
sparql_client               = PySparqlClient(
query_endpoint              = a_box_updates_config.SPARQL_QUERY_ENDPOINT    ,
update_endpoint             = a_box_updates_config.SPARQL_UPDATE_ENDPOINT   ,
kg_user                     = a_box_updates_config.KG_USERNAME              ,
kg_password                 = a_box_updates_config.KG_PASSWORD              ,
fs_url                      = ""                                            ,
fs_user                     = ""                                            ,
fs_pwd                      = ""
)