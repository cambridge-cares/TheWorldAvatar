from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.agent import default
from vapourtecagent.conf import config_vapourtec_agent


def create_app():
    vapourtec_agent_config = config_vapourtec_agent()

    agent = VapourtecAgent(
        vapourtec_digital_twin=vapourtec_agent_config.VAPOURTEC_DIGITAL_TWIN,
        vapourtec_state_periodic_timescale=vapourtec_agent_config.VAPOURTEC_STATE_PERIODIC_TIMESCALE,
        vapourtec_ip_address=vapourtec_agent_config.VAPOURTEC_IP_ADDRESS,
        fcexp_file_container_folder=vapourtec_agent_config.FCEXP_FILE_CONTAINER_FOLDER,
        fcexp_file_host_folder=vapourtec_agent_config.FCEXP_FILE_HOST_FOLDER,
        fcexp_template_filename=vapourtec_agent_config.FCEXP_TEMPLATE_FILENAME,
        dry_run=vapourtec_agent_config.DRY_RUN,
        register_agent=vapourtec_agent_config.REGISTER_AGENT,
        agent_iri=vapourtec_agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=vapourtec_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=vapourtec_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=vapourtec_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=vapourtec_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=vapourtec_agent_config.KG_USERNAME,
        kg_password=vapourtec_agent_config.KG_PASSWORD,
        fs_url=vapourtec_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=vapourtec_agent_config.FILE_SERVER_USERNAME,
        fs_password=vapourtec_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=vapourtec_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        logger_name='prod',
        max_thread_monitor_async_derivations=vapourtec_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
        email_recipient=vapourtec_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=vapourtec_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=vapourtec_agent_config.EMAIL_USERNAME,
        email_auth_json_path=vapourtec_agent_config.EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=vapourtec_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.start_all_periodical_job()

    return agent.app
