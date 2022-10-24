# Avoid unnecessary logging information from py4j package
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

# Import agent, root web page, and configuration
from derivationagentpythonexample.agent import ExampleAgent
from derivationagentpythonexample.agent import default
from derivationagentpythonexample.conf import config_example_agent

def create_app():
    # Get configurations from environment variables or env_file
    # Here we use the config_example_agent specific to this example
    # One may use the config_derivation_agent from pyderivationagent if no specific configurations are needed
    agent_config = config_example_agent()

    # Create an agent instance
    agent = ExampleAgent(
        # your custom configurations here
        example_conf_param=agent_config.EXAMPLE_CONF_PARAM,
        # other configurations from pyderivationagent
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=agent_config.KG_USERNAME,
        kg_password=agent_config.KG_PASSWORD,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        register_agent=agent_config.REGISTER_AGENT,
        logger_name="prod",
        max_thread_monitor_async_derivations=agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
        email_recipient=agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=agent_config.EMAIL_USERNAME,
        email_auth_json_path=agent_config.EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )

    # Add a root web page to the agent
    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    # Start all periodical jobs
    agent.start_all_periodical_job()

    # Expose the agent's flask app for docker
    return agent.app
