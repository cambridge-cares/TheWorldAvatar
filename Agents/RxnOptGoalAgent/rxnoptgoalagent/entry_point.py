from rxnoptgoalagent.conf import config_rxn_opt_goal_agent
from rxnoptgoalagent.agent import *

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)

from flask import Flask
import os

def create_app():
    rxn_opt_goal_agent_config = config_rxn_opt_goal_agent('./tests/env_files/agent.goal.env.test')

    agent = RxnOptGoalAgent(
        goal_agent_iri=rxn_opt_goal_agent_config.GOAL_ONTOAGENT_SERVICE_IRI,
        goal_agent_endpoint=rxn_opt_goal_agent_config.GOAL_ONTOAGENT_OPERATION_HTTP_URL,
        goal_monitor_time_interval=rxn_opt_goal_agent_config.GOAL_MONITOR_PERIODIC_TIMESCALE,
        derivation_instance_base_url=rxn_opt_goal_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=rxn_opt_goal_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=rxn_opt_goal_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=rxn_opt_goal_agent_config.KG_USERNAME,
        kg_password=rxn_opt_goal_agent_config.KG_PASSWORD,
        fs_url=rxn_opt_goal_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=rxn_opt_goal_agent_config.FILE_SERVER_USERNAME,
        fs_password=rxn_opt_goal_agent_config.FILE_SERVER_PASSWORD,
        # register_agent=rxn_opt_goal_agent_config.REGISTER_AGENT,
        logger_name="dev",
        app=Flask(__name__, template_folder=os.path.join(os.path.dirname(os.path.dirname(os.path.realpath(__file__))), "templates")),
    )

    return agent.app

if __name__ == '__main__':
    app = create_app()
    app.run(debug=True)
