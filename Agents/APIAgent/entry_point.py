from api_agent.api_agent import APIAgent
from confs.apiagent_conf import config_api_agent
from flask import request
agent_config = config_api_agent('agent.env.run')

def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is a generic API agent that manages auto-instantiation and update of Timeseries data from external data API."
    return msg




def create_app():
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
    app = agent.app
    def check_API_exist():
        request_json = request.json
        iri = request_json['api_iri']
        return {'result':agent.check_if_api_registered(iri)}
    agent.add_url_pattern('/', 'root', default, methods=['GET']) # Add root web to be introduction page
    agent.add_url_pattern('/check_api', 'check_api', check_API_exist, methods=['GET']) # Add root web to be introduction page
    agent.init_register_all_api_in_kg() # Necessary Init to retreive all API info from KG
    agent.start_all_periodical_job()
    return app