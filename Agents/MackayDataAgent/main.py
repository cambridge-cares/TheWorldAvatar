#With a config list
##setup TS
##retreive raw data
from data_agent import MackayDataAgent
agent = MackayDataAgent()
#agent.register_timeseries()
agent.update_from_external_and_predict()