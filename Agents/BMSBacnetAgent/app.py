from config.config import *
from agents.bms_bacnet_agent import bmsBacnetAgent
agent = bmsBacnetAgent()
agent.init()
agent.run(UPDATE_DURATION)