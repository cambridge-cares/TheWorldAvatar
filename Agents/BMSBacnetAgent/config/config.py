import os
from pathlib import Path


###DIRECTORIES
ROOT_DIR = Path(__file__).parent.parent
BMS_DEVICE_FILE = os.path.abspath(os.path.join(ROOT_DIR, "files", "dummyBMSDeviceMap.csv"))

###BACNET related properties
BACNET_IP = "10.10.9.7/16:47808"

####JAVA Module realted Properties
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "resources", "ts_example.properties"))
DB_URL_KEY = "db.url"
DB_USER_KEY = "db.user"
DB_PASSWORD_KEY = "db.password"


##AGENT properties
DB_TABLE = "bms"
UPDATE_DURATION = 5 # min
