from rxnoptgoalagent.conf import config_rxn_opt_goal_agent
from rxnoptgoalagent.agent import RxnOptGoalAgent
from upload_triples import config

from flask import Flask
import os

def create_app():
    rxn_opt_goal_agent_config = config_rxn_opt_goal_agent('./env_files/agent.goal.env.deploy')

    agent = RxnOptGoalAgent(
        goal_agent_iri=rxn_opt_goal_agent_config.GOAL_ONTOAGENT_SERVICE_IRI,
        goal_agent_endpoint=rxn_opt_goal_agent_config.GOAL_ONTOAGENT_OPERATION_HTTP_URL,
        goal_monitor_time_interval=rxn_opt_goal_agent_config.GOAL_MONITOR_PERIODIC_TIMESCALE,
        goal_iter_agent_iri=rxn_opt_goal_agent_config.GOAL_ITER_AGENT_IRI,
        derivation_instance_base_url=rxn_opt_goal_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=config.SPARQL_UPDATE_ENDPOINT,
        kg_user=config.KG_USERNAME,
        kg_password=config.KG_PASSWORD,
        fs_url=config.FILE_SERVER_ENDPOINT,
        fs_user=config.FILE_SERVER_USERNAME,
        fs_password=config.FILE_SERVER_PASSWORD,
        logger_name="dev",
        app=Flask(__name__, template_folder=os.path.join(os.path.dirname(os.path.realpath(__file__)), "templates")),
        email_recipient=config.EMAIL_RECIPIENT,
        email_subject_prefix=rxn_opt_goal_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=config.EMAIL_USERNAME,
        email_auth_json_path=rxn_opt_goal_agent_config.EMAIL_AUTH_JSON_PATH,
        email_goal_iteration_progress=rxn_opt_goal_agent_config.EMAIL_GOAL_ITERATION_PROGRESS,
    )

    return agent.app

if __name__ == '__main__':
    app = create_app()
    app.run()
