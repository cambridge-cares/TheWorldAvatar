from pyderivationagent.conf import Config
from pyderivationagent.conf import config_generic


class RxnOptGoalAgentConfig(Config):
    """
    This is a config class for the RxnOptGoalAgent.
    It is a subclass of Config and can be extended to provide custom configurations for developed agents.
    It has the following fields:
      - ONTOAGENT_SERVICE_IRI: The IRI of the OntoAgent:Service for the configured agent.
      - DERIVATION_PERIODIC_TIMESCALE: The time scale of the periodic job that monitors asynchronous derivations.
      - DERIVATION_INSTANCE_BASE_URL: The base URL of the derivation instances that to be created by this agent.
      - SPARQL_QUERY_ENDPOINT: The SPARQL endpoint to be used for querying the knowledge graph.
      - SPARQL_UPDATE_ENDPOINT: The SPARQL endpoint to be used for updating the knowledge graph.
      - KG_USERNAME: The username to access the SPARQL endpoint.
      - KG_PASSWORD: The password to access the SPARQL endpoint.
      - FILE_SERVER_ENDPOINT: The endpoint of the file server.
      - FILE_SERVER_USERNAME: The username to access the file server.
      - FILE_SERVER_PASSWORD: The password to access the file server.
      - ONTOAGENT_OPERATION_HTTP_URL: The URL of the OntoAgent:Operation HTTP endpoint.
      - REGISTER_AGENT: Whether to register the OntoAgent instance of the configured agent to knowledge graph.
    """
    GOAL_ONTOAGENT_SERVICE_IRI: str
    GOAL_ONTOAGENT_OPERATION_HTTP_URL: str
    GOAL_MONITOR_PERIODIC_TIMESCALE: int
    DERIVATION_INSTANCE_BASE_URL: str
    SPARQL_QUERY_ENDPOINT: str
    SPARQL_UPDATE_ENDPOINT: str
    KG_USERNAME: str
    KG_PASSWORD: str
    FILE_SERVER_ENDPOINT: str
    FILE_SERVER_USERNAME: str
    FILE_SERVER_PASSWORD: str
    # REGISTER_AGENT: bool

def config_rxn_opt_goal_agent(env_file: str = None) -> RxnOptGoalAgentConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(RxnOptGoalAgentConfig, env_file)
