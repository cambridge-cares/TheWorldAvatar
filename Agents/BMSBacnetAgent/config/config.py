import os
from pathlib import Path


###DIRECTORIES
ROOT_DIR = Path(__file__).parent.parent
BMS_DEVICE_FILE = os.path.abspath(os.path.join(ROOT_DIR, "files", "bmsbacnetmap.csv"))
DEVICE_NAME_MAPPING = BMS_DEVICE_FILE
TEMPLATE_DIR = os.path.abspath(os.path.join(ROOT_DIR, "files", "rdf-xml-templates"))
###BACNET related properties
BACNET_IP = "10.10.0.3/16:47808"
DEVICE_ID = "10.10.11.61"

####JAVA Module realted Properties
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "ts_example.properties"))
DB_URL_KEY = "db.url"
DB_URL_PYSCOPG = "db.url.pyscopg"
DB_HOST_KEY = "db.host"
DB_USER_KEY = "db.user"
DB_PASSWORD_KEY = "db.password"
ENDPOINT_KEY = "sparql.query.endpoint"
DB_PORT = "db.port"



##AGENT properties
DB_TABLE = "bms"
UPDATE_DURATION = 1 # min
DOMAIN = 'https://www.theworldavatar.com/kg/ontobms/'
