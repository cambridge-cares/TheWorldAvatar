from pyderivationagent.conf import AgentConfig
from pyderivationagent.conf import config_generic

# The purpose of this module is to provide the config class for your agent-specific config parameters
# The config class should be based on the pyderivationagent.conf.AgentConfig class
# Also, you need to provide a config function to return the config class

class APIAgentConfig(AgentConfig):
    DB_URL: str
    DB_USER:str
    DB_PW:str


def config_api_agent(env_file: str = None):
    return config_generic(APIAgentConfig, env_file)