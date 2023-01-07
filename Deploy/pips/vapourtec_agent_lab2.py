from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.agent import default
from vapourtecagent.conf import config_vapourtec_agent
from upload_triples import config

def create_app():
    vapourtec_agent_config = config_vapourtec_agent('./env_files/agent.lab2.vapourtec.env.deploy')

    agent = VapourtecAgent(
        vapourtec_digital_twin=vapourtec_agent_config.VAPOURTEC_DIGITAL_TWIN,
        vapourtec_state_periodic_timescale=vapourtec_agent_config.VAPOURTEC_STATE_PERIODIC_TIMESCALE,
        # NOTE below vapourtec_ip_address is commented out on purpose
        # developer should populate this value in the env file with the value returned by running below command in WSL2
        # echo $(ipconfig.exe | grep 'vEthernet (WSL)' -A4 | cut -d":" -f 2 | tail -n1 | sed -e 's/\s*//g')
        # then uncomment the line below
        vapourtec_ip_address=#vapourtec_agent_config.VAPOURTEC_IP_ADDRESS,
        fcexp_file_container_folder=vapourtec_agent_config.FCEXP_FILE_CONTAINER_FOLDER,
        fcexp_file_host_folder=vapourtec_agent_config.FCEXP_FILE_HOST_FOLDER,
        fcexp_template_filename=vapourtec_agent_config.FCEXP_TEMPLATE_FILENAME,
        dry_run=vapourtec_agent_config.DRY_RUN,
        register_agent=vapourtec_agent_config.REGISTER_AGENT,
        agent_iri=vapourtec_agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=vapourtec_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=vapourtec_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=config.SPARQL_UPDATE_ENDPOINT,
        kg_user=config.KG_USERNAME,
        kg_password=config.KG_PASSWORD,
        fs_url=config.FILE_SERVER_ENDPOINT,
        fs_user=config.FILE_SERVER_USERNAME,
        fs_password=config.FILE_SERVER_PASSWORD,
        agent_endpoint=vapourtec_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        logger_name='dev',
        max_thread_monitor_async_derivations=vapourtec_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
        email_recipient=config.EMAIL_RECIPIENT,
        email_subject_prefix=vapourtec_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=config.EMAIL_USERNAME,
        email_auth_json_path=vapourtec_agent_config.EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=vapourtec_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.start_all_periodical_job()

    return agent.app


if __name__ == '__main__':
    app = create_app()
    app.run(port=7003)
