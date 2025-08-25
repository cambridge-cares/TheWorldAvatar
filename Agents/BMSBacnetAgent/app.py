from config.config import *
import agents.bms_bacnet_agent
from jpsAccess.tsclient_wrapper import create_postgres_db_if_not_exis
from config.config import *
from config.parsedConfig import *
import schedule
import time
from deviceMap.bms_bacnet_dictionary import *

agent = agents.bms_bacnet_agent.bmsBacnetAgent()
#schedule.every(UPDATE_DURATION).minutes.do(agent.updateKB)
schedule.every(50).seconds.do(agent.updateKB)
while True:
    schedule.run_pending()
    time.sleep(30)