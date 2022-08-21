from abc import ABC
from flask_apscheduler import APScheduler
from flask import Flask
from flask import request
from flask import render_template
from urllib.parse import unquote
from urllib.parse import urlparse
import json
import time
import os

import agentlogging

from rxnoptgoalagent.kg_operations import RxnOptGoalSparqlClient


class FlaskConfig(object):
    """
        This class provides the configuration for flask app object. Each config should be provided as constant. For more information, visit https://flask.palletsprojects.com/en/2.0.x/config/.
    """
    SCHEDULER_API_ENABLED = True


class RxnOptGoalAgent(ABC):
    def __init__(
        self,
        goal_agent_iri: str,
        goal_agent_endpoint: str,
        goal_monitor_time_interval: int,
        derivation_instance_base_url: str,
        kg_url: str,
        kg_update_url: str = None,
        kg_user: str = None,
        kg_password: str = None,
        fs_url: str = None,
        fs_user: str = None,
        fs_password: str = None,
        app: Flask = Flask(__name__, template_folder="/app/templates"),
        flask_config: FlaskConfig = FlaskConfig(),
        # register_agent: bool = True,
        logger_name: str = "dev"
    ):
        """
            This method initialises the instance of RxnOptGoalAgent.

            Arguments:
                app - flask app object, an example: app = Flask(__name__)
                goal_agent_iri - OntoAgent:Service IRI of the goal agent, an example: "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
                goal_agent_endpoint - data property OntoAgent:hasHttpUrl of OntoAgent:Operation of the derivation agent, an example: "http://localhost:5000/endpoint"
                goal_monitor_time_interval - time interval between two runs of goal monitoring job (in SECONDS)
                derivation_instance_base_url - namespace to be used when creating derivation instance, an example: "http://www.example.com/triplestore/repository/"
                kg_url - SPARQL query endpoint, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_update_url - SPARQL update endpoint, will be set to the same value as kg_url if not provided, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_user - username used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                kg_password - password that set for the kg_user used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                fs_url - file server endpoint, an example: "http://localhost:8080/FileServer/"
                fs_user - username used to access the file server endpoint specified by fs_url
                fs_password - password that set for the fs_user used to access the file server endpoint specified by fs_url
                flask_config - configuration object for flask app, should be an instance of the class FlaskConfig provided as part of this package
                #TODO register_agent - boolean value, whether to register the agent to the knowledge graph
                logger_name - logger names for getting correct loggers from agentlogging package, valid logger names: "dev" and "prod", for more information, visit https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/utils/python-utils/agentlogging/logging.py
        """

        # initialise flask app with its configuration
        self.app = app
        self.app.config.from_object(flask_config)

        # initialise flask scheduler and assign time interval for monitorDerivations job
        self.scheduler = APScheduler(app=self.app)
        self.goal_monitor_time_interval = goal_monitor_time_interval

        # assign IRI and HTTP URL of the agent
        self.goal_agent_iri = goal_agent_iri
        self.goal_agent_endpoint = goal_agent_endpoint

        # assign KG related information
        self.kg_url = kg_url
        self.kg_update_url = kg_update_url if kg_update_url is not None else kg_url
        self.kg_user = kg_user
        self.kg_password = kg_password

        # assign file server related information
        self.fs_url = fs_url
        self.fs_user = fs_user
        self.fs_password = fs_password

        # initialise the RxnOptGoalSparqlClient
        self.sparql_client = RxnOptGoalSparqlClient(
            query_endpoint=self.kg_url, update_endpoint=self.kg_update_url,
            kg_user=self.kg_user, kg_password=self.kg_password,
            fs_url=self.fs_url, fs_user=self.fs_user, fs_pwd=self.fs_password
        )

        # initialise the derivation_client with SPARQL Query and Update endpoint
        if kg_user is None:
            self.store_client = self.sparql_client.jpsBaseLib_view.RemoteStoreClient(
                self.kg_url, self.kg_update_url)
        else:
            self.store_client = self.sparql_client.jpsBaseLib_view.RemoteStoreClient(
                self.kg_url, self.kg_update_url, self.kg_user, self.kg_password)
        self.derivation_client = self.sparql_client.jpsBaseLib_view.DerivationClient(
            self.store_client, derivation_instance_base_url)

        # initialise the logger
        self.logger = agentlogging.get_logger(logger_name)

        # add the default routes for the flask app
        self.app.add_url_rule('/', 'root', self.default, methods=['GET'])

        # add the route for the rxn opt goal specification
        self.app.add_url_rule('/goal', 'rxn_opt_goal', self.goal_page, methods=['GET'])

        # add the url pattern that handles the goal request
        url_pattern = urlparse(self.goal_agent_endpoint).path
        url_pattern_name = url_pattern.strip('/').replace('/', '_') + '_rxnoptgoal'
        self.app.add_url_rule(url_pattern, url_pattern_name, self.handle_rxn_opt_goal_request, methods=['POST'])
        self.logger.info(f"The endpoint to handle goal request is added as: {url_pattern}")

        # # register the agent to the KG if required
        # self.register_agent = register_agent
        # try:
        #     self.register_agent_in_kg()
        # except Exception as e:
        #     self.logger.error(
        #         "Failed to register the agent <{}> to the KG <{}>. Error: {}".format(self.agentIRI, self.kg_url, e),
        #         stack_info=True, exc_info=True)
        #     raise e

        self.logger.info(f"RxnOptGoalAgent initialised with IRI: {self.goal_agent_iri}")

    def default(self):
        """Instruction for the RxnOptGoalAgent usage."""
        msg = "Welcome to the RxnOptGoalAgent!<BR>"
        msg += "This is a goal agent that capable of persure a reaction optimisation goal.<BR>"
        msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/160-dev-rxn-opt-goal-agent/Agents/RxnOptGoalAgent#readme<BR>"    
        return msg

    def goal_page(self):
        return render_template('rxn_opt_goal.html')

    def handle_rxn_opt_goal_request(self):
        """
        This function is called when a goal request is received.
        """
        self.logger.info(f"Received a goal request: {request.url}")
        return "OK"
