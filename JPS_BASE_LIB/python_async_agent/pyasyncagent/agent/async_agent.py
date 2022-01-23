from flask_apscheduler import APScheduler
from flask import Flask
import json

import agentlogging

from pyasyncagent.kg_operations import *

class FlaskConfig(object):
    """
        This class provides the configuration for flask app object. Each config should be provided as constant. For more information, visit https://flask.palletsprojects.com/en/2.0.x/config/.
    """
    SCHEDULER_API_ENABLED = True

class AsyncAgent(object):
    def __init__(
        self,
        agent_iri: str,
        time_interval: int,
        derivation_instance_base_url: str,
        kg_url: str,
        kg_user: str = None,
        kg_password: str = None,
        app: Flask = Flask(__name__),
        flask_config: FlaskConfig = FlaskConfig(),
        logger_name: str = "dev"):
        """
            This method initialises the asynchronous agent.

            Arguments:
                app - flask app object, an example: app = Flask(__name__)
                agent_iri - OntoAgent:Service IRI of the asynchronous agent, an example: "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
                time_interval - time interval between two runs of derivation monitoring job (in SECONDS)
                derivation_instance_base_url - namespace to be used when creating derivation instance, an example: "http://www.example.com/triplestore/repository/"
                kg_url - SPARQL query/update endpoint, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_user - username used to access the SPARQL query/update endpoint specified by kg_url
                kg_password - password that set for the kg_user used to access the SPARQL query/update endpoint specified by kg_url
                flask_config - configuration object for flask app, should be an instance of the class FlaskConfig provided as part of this package
                logger_name - logger names for getting correct loggers from agentlogging package, valid logger names: "dev" and "prod", for more information, visit https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/utils/python-utils/agentlogging/logging.py
        """

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

        # initialise flask app with its configuration
        self.app = app
        self.app.config.from_object(flask_config)

        # initialise flask scheduler and assign time interval for monitorDerivations job
        self.scheduler = APScheduler()
        self.time_interval = time_interval

        # assign IRI of the agent
        self.agentIRI = agent_iri

        # assign KG related information
        self.kgUrl = kg_url
        self.kgUser = kg_user
        self.kgPassword = kg_password

        # initialise the derivationClient with SPARQL Query and Update endpoint
        if self.kgUrl is None:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.kgUrl, self.kgUrl)
        else:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.kgUrl, self.kgUrl, self.kgUser, self.kgPassword)
        self.derivationClient = self.jpsBaseLib_view.DerivationClient(self.storeClient, derivation_instance_base_url)

        self.logger = agentlogging.get_logger(logger_name)
        self.logger.info(
            "AsyncAgent <%s> is initialised to monitor derivations in triple store <%s> with a time interval of %d seconds." % (self.agentIRI, self.kgUrl, self.time_interval)
        )

    def add_url_pattern(self, url_pattern=None, url_pattern_name=None, function=None, methods=['GET'], *args, **kwargs):
        """
            This method is a wrapper of add_url_rule method of Flask object that adds customised URL Pattern to asynchronous agent.
            For more information, visit https://flask.palletsprojects.com/en/2.0.x/api/#flask.Flask.add_url_rule.

            Arguments:
                url_pattern - the endpoint url to associate with the rule and view function
                url_pattern_name - the name of the endpoint
                function - the view function to associate with the endpoint
                methods - HTTP request methods, default to ['GET']
        """
        self.app.add_url_rule(url_pattern, url_pattern_name, function, methods=methods, *args, **kwargs)
        self.logger.info("A URL Pattern <%s> is added." % (url_pattern))

    def monitorDerivations(self):
        """
            This method monitors the status of the derivation that "isDerivedUsing" AsyncAgent.

            When it detects the status is "PendingUpdate", the agent will check its immediate upstream derivations.
            Once all its immediate upstream derivations are up-to-date, the agent marks the status as "Requested".

            When it detects the status is "Requested", the agent will mark the status as "InProgress" and start the job.
            Once the job is finished, the agent marks the status as "Finished" and attaches the new derived IRI to it via "hasNewDerivedIRI".

            When it detects the status is "InProgress", the currently implementation just passes.

            When it detects the status is "Finished", the agent deletes the old entities,
            reconnects the new instances (previously attached to the status via "hasNewDerivedIRI") with the original derivation,
            cleans up all the status, and finally updates the timestamp of the derivation.
            All these processing steps at the `Finished` status are taken care of by method
            `uk.ac.cam.cares.jps.base.derivation.DerivationClient.cleanUpFinishedDerivationUpdate(String)`.
        """

        # Below codes follow the logic as defined in AsynAgent.java in JPS_BASE_LIB
        # for more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/agent/AsynAgent.java

        # Retrieves a list of derivations and their status type that "isDerivedUsing" AsyncAgent
        derivationAndStatusType = self.derivationClient.getDerivationsAndStatusType(self.agentIRI)
        if bool(derivationAndStatusType):
            self.logger.info("A list of derivations that <isDerivedUsing> <%s> are retrieved: %s." % (self.agentIRI, derivationAndStatusType))

            # Iterate over the list of derivation, and do different things depend on the derivation status
            for derivation in derivationAndStatusType:
                statusType = str(derivationAndStatusType[derivation])
                self.logger.info("Derivation <%s> has status type: %s." % (derivation, statusType))

                # If "PendingUpdate", check the immediate upstream derivations if they are up-to-date
                if statusType == 'PENDINGUPDATE':
                    immediateUpstreamDerivationToUpdate = self.derivationClient.checkAtPendingUpdate(derivation)
                    if immediateUpstreamDerivationToUpdate is not None:
                        self.logger.info(
                            "Derivation <%s> has a list of immediate upstream derivations to be updated: <%s>." % (derivation, ">, <".join(immediateUpstreamDerivationToUpdate))
                        )
                    else:
                        self.logger.info("All immediate upstream derivations of derivation <%s> are up-to-date." % (derivation))

                # If "Requested", retrieve inputs, marks as "InProgress", start job, update status at job completion
                elif statusType == 'REQUESTED':
                    agentInputs = str(self.derivationClient.retrieveAgentInputIRIs(derivation, self.agentIRI))
                    self.logger.info("Agent <%s> retrieved inputs of derivation <%s>: %s." % (self.agentIRI, derivation, agentInputs))
                    self.logger.info("Derivation <%s> is in progress." % (derivation))

                    # Preprocessing inputs to be sent to agent for setting up job, this is now in dict datatype
                    agent_input_json = json.loads(agentInputs) if not isinstance(agentInputs, dict) else agentInputs
                    agent_input_key = str(self.jpsBaseLib_view.DerivationClient.AGENT_INPUT_KEY)
                    if agent_input_key in agent_input_json:
                        inputs_to_send = agent_input_json[agent_input_key]
                    else:
                        self.logger.error("Agent input key (%s) might be missing. Received input: %s." % (agent_input_key, agent_input_json.__dict__))
                    # The inputs_to_send should be a dictionary format,
                    # for example: {'OntoXX:Concept_A': 'Instance_A', 'OntoXX:Concept_B': 'Instance_B'}
                    # Developer can directly use it with dictionary operations
                    newDerivedIRI = self.setupJob(inputs_to_send)
                    self.logger.info("Derivation <%s> generated new derived IRI: <%s>." % (derivation, ">, <".join(newDerivedIRI)))

                    self.derivationClient.updateStatusAtJobCompletion(derivation, newDerivedIRI)
                    self.logger.info("Derivation <%s> is now finished, to be cleaned up." % (derivation))

                # If "InProgress", pass
                elif statusType == 'INPROGRESS':
                    pass

                # If "Finished", do all the clean-up steps
                elif statusType == 'FINISHED':
                    self.derivationClient.cleanUpFinishedDerivationUpdate(derivation)
                    self.logger.info("Derivation <%s> is now cleand up." % (derivation))

                # If anything else, pass
                else:
                    self.logger.info("Derivation <%s> has unhandled status type: %s." % (derivation, statusType))
                    pass

        else:
            self.logger.info("Currently, no derivation <isDerivedUsing> <%s>." % (self.agentIRI))

    def setupJob(self, agentInputs) -> list:
        """
            This method sets up the job to update the derivation. Developer shall override this when writing new asynchronous agent based on AsyncAgent class.

            Arguments:
                agentInputs - a JSON/dictionary of the derivation inputs, an example:
                    {
                        "https://www.example.com/triplestore/repository/Ontology.owl#Concept_1": "https://www.example.com/triplestore/repository/Concept_1/Instance_1",
                        "https://www.example.com/triplestore/repository/Ontology.owl#Concept_2": "https://www.example.com/triplestore/repository/Concept_2/Instance_2",
                        "https://www.example.com/triplestore/repository/Ontology.owl#Concept_3":
                            ["https://www.example.com/triplestore/repository/Concept_3/Instance_3_1", "https://www.example.com/triplestore/repository/Concept_3/Instance_3_2"],
                        "https://www.example.com/triplestore/repository/Ontology.owl#Concept_4": "https://www.example.com/triplestore/repository/Concept_4/Instance_4"
                    }
        """
        createdIRI = []
        return createdIRI

    def run(self, **kwargs):
        """
            This method starts the periodical job to monitor derivation, also runs the flask app as an HTTP servlet.
        """
        self.scheduler.init_app(self.app)
        self.scheduler.add_job(id='monitor_derivations', func=self.monitorDerivations, trigger='interval', seconds=self.time_interval)
        self.scheduler.start()
        self.logger.info("Monitor derivations job is started with a time interval of %d seconds." % (self.time_interval))
        self.app.run(**kwargs)
