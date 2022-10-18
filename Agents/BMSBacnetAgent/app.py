from config.config import *
import agents.bms_bacnet_agent
from jpsAccess.tsclient_wrapper import create_postgres_db_if_not_exis
from config.config import *
from config.parsedConfig import *
import schedule
import time
agent = agents.bms_bacnet_agent.bmsBacnetAgent()
agent.connect()
schedule.every(10).minutes.do(agent.updateKB)
while True:
    schedule.run_pending()
    time.sleep(1)