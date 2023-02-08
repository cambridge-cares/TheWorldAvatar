from pyderivationagent.conf import config_derivation_agent

from rxnoptgoaliteragent.agent import RxnOptGoalIterAgent
from rxnoptgoaliteragent.agent import default


def create_app():
    agent_config = config_derivation_agent()

    agent = RxnOptGoalIterAgent(
        register_agent=agent_config.REGISTER_AGENT,
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=agent_config.KG_USERNAME,
        kg_password=agent_config.KG_PASSWORD,
        fs_url=agent_config.FILE_SERVER_ENDPOINT,
        fs_user=agent_config.FILE_SERVER_USERNAME,
        fs_password=agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        logger_name='prod',
        max_thread_monitor_async_derivations=agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
        email_recipient=agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=agent_config.EMAIL_USERNAME,
        email_auth_json_path=agent_config.EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.start_all_periodical_job()

    return agent.app
