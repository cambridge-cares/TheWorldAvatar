from pyderivationagent.conf import Config
from pyderivationagent.conf import config_generic


class RxnOptGoalAgentConfig(Config):
    """
    This is a config class for the RxnOptGoalAgent.
    It is a subclass of Config and can be extended to provide custom configurations for developed agents.
    It has the following fields:
      - GOAL_ONTOAGENT_SERVICE_IRI: OntoAgent:Service IRI of the goal agent.
      - GOAL_ONTOAGENT_OPERATION_HTTP_URL: HTTP URL of the goal agent which takes goal request via HTTP POST.
      - GOAL_MONITOR_PERIODIC_TIMESCALE: Periodic timescale for monitoring active goal set.
      - GOAL_ITER_AGENT_IRI: OntoAgent:Service IRI of the RxnOptGoalIter agent which the goal agent delegates the work to.
      - DERIVATION_INSTANCE_BASE_URL: The base URL of the derivation instances that to be created by this agent.
      - SPARQL_QUERY_ENDPOINT: The SPARQL endpoint to be used for querying the knowledge graph.
      - SPARQL_UPDATE_ENDPOINT: The SPARQL endpoint to be used for updating the knowledge graph.
      - KG_USERNAME: The username to access the SPARQL endpoint.
      - KG_PASSWORD: The password to access the SPARQL endpoint.
      - FILE_SERVER_ENDPOINT: The endpoint of the file server.
      - FILE_SERVER_USERNAME: The username to access the file server.
      - FILE_SERVER_PASSWORD: The password to access the file server.
      - EMAIL_RECIPIENT: The email address to send emails to.
      - EMAIL_SUBJECT_PREFIX: The prefix of the subject of the emails.
      - EMAIL_USERNAME: The username to access the email server.
      - EMAIL_AUTH_JSON_PATH: The path to the JSON file containing the authentication information for the email server.
      - EMAIL_GOAL_ITERATION_PROGRESS: Whether to send emails to the recipient when a goal iteration enters the next iteration.
    """
    GOAL_ONTOAGENT_SERVICE_IRI: str
    GOAL_ONTOAGENT_OPERATION_HTTP_URL: str
    GOAL_MONITOR_PERIODIC_TIMESCALE: int
    GOAL_ITER_AGENT_IRI: str
    DERIVATION_INSTANCE_BASE_URL: str
    SPARQL_QUERY_ENDPOINT: str
    SPARQL_UPDATE_ENDPOINT: str
    KG_USERNAME: str
    KG_PASSWORD: str
    FILE_SERVER_ENDPOINT: str
    FILE_SERVER_USERNAME: str
    FILE_SERVER_PASSWORD: str
    EMAIL_RECIPIENT: str = ''
    EMAIL_SUBJECT_PREFIX: str = ''
    EMAIL_USERNAME: str = ''
    EMAIL_AUTH_JSON_PATH: str = ''
    EMAIL_GOAL_ITERATION_PROGRESS: bool = False

def config_rxn_opt_goal_agent(env_file: str = None) -> RxnOptGoalAgentConfig:
    """Return configurations from either environment variables or env_file."""
    return config_generic(RxnOptGoalAgentConfig, env_file)
