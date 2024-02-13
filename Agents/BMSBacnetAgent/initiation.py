'''
This script handles everything about bms data initiation.
it is designed to run separately. One this is setup, BMS agent should be able to continue update the sensor data.
'''
import agents.bms_bacnet_agent
from jpsAccess.tsclient_wrapper import create_postgres_db
from config.config import *
from config.parsedConfig import *
def init():
    #Create central table in postgresqk of sensor timeseries data if not exist yet
    JAVA_PROPERTIES = readProperties()
    create_postgres_db(JAVA_PROPERTIES[DB_TABLE],JAVA_PROPERTIES[DB_USER_KEY], JAVA_PROPERTIES[DB_PASSWORD_KEY], JAVA_PROPERTIES[DB_URL_KEY])
    #Run the initiation of bmsagent which insert the time series reference triple into blazegraph
    agent = agents.bms_bacnet_agent.bmsBacnetAgent()
    agent.init()

if __name__ == "__main__":
    init()