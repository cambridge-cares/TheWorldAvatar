from hplcagent.agent import HPLCAgent
from hplcagent.agent import default
from hplcagent.conf import config_hplc_agent


def create_app():
    hplc_agent_config = config_hplc_agent()

    agent = HPLCAgent(
        hplc_digital_twin=hplc_agent_config.HPLC_DIGITAL_TWIN,
        hplc_report_periodic_timescale=hplc_agent_config.HPLC_REPORT_PERIODIC_TIMESCALE,
        hplc_report_container_dir=hplc_agent_config.HPLC_REPORT_CONTAINER_DIR,
        current_hplc_method=hplc_agent_config.CURRENT_HPLC_METHOD,
        hplc_report_file_extension=hplc_agent_config.HPLC_REPORT_FILE_EXTENSION,
        register_agent=hplc_agent_config.REGISTER_AGENT,
        agent_iri=hplc_agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=hplc_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=hplc_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=hplc_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=hplc_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=hplc_agent_config.KG_USERNAME,
        kg_password=hplc_agent_config.KG_PASSWORD,
        fs_url=hplc_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=hplc_agent_config.FILE_SERVER_USERNAME,
        fs_password=hplc_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=hplc_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        logger_name='prod',
        dry_run=hplc_agent_config.DRY_RUN,
        max_thread_monitor_async_derivations=hplc_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
        email_recipient=hplc_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=hplc_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=hplc_agent_config.EMAIL_USERNAME,
        email_auth_json_path=hplc_agent_config.EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=hplc_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.start_all_periodical_job()

    return agent.app
