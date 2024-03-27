from api_agent.api_agent import APIAgent
from confs.apiagent_conf import config_api_agent

agent_config = config_api_agent('agent.env.run')

def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is a derivation agent that serve as an example of pyderivationagent package.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DerivationAgentPythonExample#readme<BR>"
    return msg

def create_app():
    print('start agebt')
    agent = APIAgent(
        db_url=agent_config.DB_URL,
        db_user=agent_config.DB_USER,
        db_pw = agent_config.DB_PW,
        agent_iri= agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval= agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url= agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url= agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=agent_config.SPARQL_QUERY_ENDPOINT,
        kg_user= agent_config.KG_USERNAME,
        kg_password = agent_config.KG_PASSWORD,
        agent_endpoint= agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        register_agent = agent_config.REGISTER_AGENT
    )
    print('started agent')
    app = agent.app
    agent.add_url_pattern('/', 'root', default, methods=['GET']) # Add root web to be introduction page
    agent.init_register_all_api_in_kg() # Necessary Init to retreive all API info from KG
    agent.start_all_periodical_job()
    return app

