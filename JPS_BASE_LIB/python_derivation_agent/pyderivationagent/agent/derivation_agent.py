from flask_apscheduler import APScheduler
from flask import Flask
from flask import request
from urllib.parse import unquote
import json
import time

import agentlogging

from pyderivationagent.kg_operations import *
from pyderivationagent.data_model import DerivationInputs, DerivationOutputs


class FlaskConfig(object):
    """
        This class provides the configuration for flask app object. Each config should be provided as constant. For more information, visit https://flask.palletsprojects.com/en/2.0.x/config/.
    """
    SCHEDULER_API_ENABLED = True


class DerivationAgent(object):
    def __init__(
        self,
        agent_iri: str,
        time_interval: int,
        derivation_instance_base_url: str,
        kg_url: str,
        kg_update_url: str = None,
        kg_user: str = None,
        kg_password: str = None,
        fs_url: str = None,
        fs_user: str = None,
        fs_password: str = None,
        app: Flask = Flask(__name__),
        flask_config: FlaskConfig = FlaskConfig(),
        agent_endpoint: str = "/",
        logger_name: str = "dev"
    ):
        """
            This method initialises the instance of DerivationAgent.

            Arguments:
                app - flask app object, an example: app = Flask(__name__)
                agent_iri - OntoAgent:Service IRI of the derivation agent, an example: "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
                agent_endpoint - data property OntoAgent:hasHttpUrl of OntoAgent:Operation of the derivation agent, an example: "http://localhost:7000/endpoint"
                time_interval - time interval between two runs of derivation monitoring job (in SECONDS)
                derivation_instance_base_url - namespace to be used when creating derivation instance, an example: "http://www.example.com/triplestore/repository/"
                kg_url - SPARQL query endpoint, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_update_url - SPARQL update endpoint, will be set to the same value as kg_url if not provided, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_user - username used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                kg_password - password that set for the kg_user used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                fs_url - file server endpoint, an example: "http://localhost:8080/FileServer/"
                fs_user - username used to access the file server endpoint specified by fs_url
                fs_password - password that set for the fs_user used to access the file server endpoint specified by fs_url
                flask_config - configuration object for flask app, should be an instance of the class FlaskConfig provided as part of this package
                logger_name - logger names for getting correct loggers from agentlogging package, valid logger names: "dev" and "prod", for more information, visit https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/utils/python-utils/agentlogging/logging.py
        """

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.agent.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.derivation.*")

        # initialise flask app with its configuration
        self.app = app
        self.app.config.from_object(flask_config)

        # initialise flask scheduler and assign time interval for monitorDerivations job
        self.scheduler = APScheduler(app=self.app)
        self.time_interval = time_interval

        # assign IRI and HTTP URL of the agent
        self.agentIRI = agent_iri
        self.agentEndpoint = agent_endpoint

        # assign KG related information
        self.kgUrl = kg_url
        self.kgUpdateUrl = kg_update_url if kg_update_url is not None else kg_url
        self.kgUser = kg_user
        self.kgPassword = kg_password

        # assign file server related information
        self.fs_url = fs_url
        self.fs_user = fs_user
        self.fs_password = fs_password

        # initialise the derivationClient with SPARQL Query and Update endpoint
        if kg_user is None:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(
                self.kgUrl, self.kgUpdateUrl)
        else:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(
                self.kgUrl, self.kgUpdateUrl, self.kgUser, self.kgPassword)
        self.derivationClient = self.jpsBaseLib_view.DerivationClient(
            self.storeClient, derivation_instance_base_url)

        self.logger = agentlogging.get_logger(logger_name)
        self.logger.info(
            "DerivationAgent <%s> is initialised to monitor derivations in triple store <%s> with a time interval of %d seconds." % (
                self.agentIRI, self.kgUrl, self.time_interval)
        )

    def add_url_pattern(self, url_pattern=None, url_pattern_name=None, function=None, methods=['GET'], *args, **kwargs):
        """
            This method is a wrapper of add_url_rule method of Flask object that adds customised URL Pattern to derivation agent.
            #flask.Flask.add_url_rule.
            For more information, visit https://flask.palletsprojects.com/en/2.0.x/api/
            WARNING: Use of this is STRONGLY discouraged. The design intention of an derivation agent is to communicate via the KNOWLEDGE GRAPH, and NOT via HTTP requests.

            Arguments:
                url_pattern - the endpoint url to associate with the rule and view function
                url_pattern_name - the name of the endpoint
                function - the view function to associate with the endpoint
                methods - HTTP request methods, default to ['GET']
        """
        self.app.add_url_rule(url_pattern, url_pattern_name,
                              function, methods=methods, *args, **kwargs)
        self.logger.info("A URL Pattern <%s> is added." % (url_pattern))

    def monitor_async_derivations(self):
        """
            This method monitors the status of the asynchronous derivation that "isDerivedUsing" DerivationAgent.

            When it detects the status is "Requested", the agent will mark the status as "InProgress" and start the job.
            Once the job is finished, the agent marks the status as "Finished" and attaches the new derived IRI to it via "hasNewDerivedIRI".
            All new generated triples are also written to the knowledge graph at this point.

            When it detects the status is "InProgress", the currently implementation just passes.

            When it detects the status is "Finished", the agent deletes the old entities,
            reconnects the new instances (previously attached to the status via "hasNewDerivedIRI") with the original derivation,
            cleans up all the status, and finally updates the timestamp of the derivation.
            All these processing steps at the `Finished` status are taken care of by method
            `uk.ac.cam.cares.jps.base.derivation.DerivationClient.cleanUpFinishedDerivationUpdate(String)`.
        """

        # Below codes follow the logic as defined in DerivationAgent.java in JPS_BASE_LIB
        # for more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/agent/DerivationAgent.java

        # Retrieves a list of derivations and their status type that "isDerivedUsing" DerivationAgent
        derivationAndStatusType = self.derivationClient.getDerivationsAndStatusType(
            self.agentIRI)
        if bool(derivationAndStatusType):
            self.logger.info("A list of asynchronous derivations that <isDerivedUsing> <%s> are retrieved: %s." % (
                self.agentIRI, {d: str(derivationAndStatusType[d]) for d in derivationAndStatusType}))

            # Iterate over the list of derivation, and do different things depend on the derivation status
            for derivation in derivationAndStatusType:
                statusType = str(derivationAndStatusType[derivation])
                self.logger.info("Asynchronous derivation <%s> has status type: %s." % (
                    derivation, statusType))

                # If "Requested", check the immediate upstream derivations if they are up-to-date
                # if any of the asynchronous derivations are still outdated, skip, otherwise, request update of all synchronous derivations
                # then retrieve inputs, marks as "InProgress", start job, update status at job completion
                if statusType == 'REQUESTED':
                    immediateUpstreamDerivationToUpdate = self.derivationClient.checkImmediateUpstreamDerivation(derivation)
                    if self.jpsBaseLib_view.DerivationSparql.ONTODERIVATION_DERIVATIONASYN in immediateUpstreamDerivationToUpdate:
                        self.logger.info("Asynchronous derivation <" + derivation
                                         + "> has a list of immediate upstream asynchronous derivations to be updated: "
                                         + str(immediateUpstreamDerivationToUpdate))
                    else:
                        syncDerivationsToUpdate = self.derivationClient.groupSyncDerivationsToUpdate(immediateUpstreamDerivationToUpdate)
                        if bool(syncDerivationsToUpdate):
                            self.logger.info("Asynchronous derivation <" + derivation
                                             + "> has a list of immediate upstream synchronous derivations to be updated: "
                                             + str(syncDerivationsToUpdate))
                            self.derivationClient.updatePureSyncDerivations(syncDerivationsToUpdate)
                            self.logger.info("Update of synchronous derivation is done for: " + str(syncDerivationsToUpdate))
                        if not bool(self.derivationClient.checkImmediateUpstreamDerivation(derivation)):
                            agentInputs = str(self.derivationClient.retrieveAgentInputIRIs(derivation, self.agentIRI))
                            self.logger.info("Agent <%s> retrieved inputs of asynchronous derivation <%s>: %s." % (
                                self.agentIRI, derivation, agentInputs))
                            self.logger.info("Asynchronous derivation <%s> is in progress." % (derivation))

                            # Preprocessing inputs to be sent to agent for setting up job, this is now in dict datatype
                            agent_input_json = json.loads(agentInputs) if not isinstance(agentInputs, dict) else agentInputs
                            agent_input_key = str(self.jpsBaseLib_view.DerivationClient.AGENT_INPUT_KEY)
                            if agent_input_key in agent_input_json:
                                inputs_to_send = agent_input_json[agent_input_key]
                            else:
                                self.logger.error("Agent input key (%s) might be missing. Received input: %s." % (
                                    agent_input_key, agent_input_json.__dict__))
                            # The inputs_to_send should be a key-values pair format,
                            # for example: {'OntoXX:Concept_A': ['Instance_A'], 'OntoXX:Concept_B': ['Instance_B']}
                            derivationInputs = self.jpsBaseLib_view.DerivationInputs(inputs_to_send)
                            derivation_inputs = DerivationInputs(derivationInputs)
                            derivationOutputs = self.jpsBaseLib_view.DerivationOutputs()
                            derivation_outputs = DerivationOutputs(derivationOutputs)
                            self.process_request_parameters(derivation_inputs, derivation_outputs)

                            newDerivedIRI = derivationOutputs.getNewDerivedIRI()
                            newTriples = derivationOutputs.getOutputTriples()
                            self.derivationClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, newTriples)
                            self.logger.info("Asynchronous derivation <%s> generated new derived IRI: <%s>." % (
                                derivation, ">, <".join(newDerivedIRI)))
                            self.logger.info("Asynchronous derivation <" + derivation +
                                             "> has all new generated triples: " + str([t.getQueryString() for t in newTriples]))
                            self.logger.info("Asynchronous derivation <" + derivation + "> is now finished, to be cleaned up.")

                # If "InProgress", pass
                elif statusType == 'INPROGRESS':
                    pass

                # If "Finished", do all the clean-up steps
                elif statusType == 'FINISHED':
                    self.derivationClient.cleanUpFinishedDerivationUpdate(derivation)
                    self.logger.info("Asynchronous derivation <%s> is now cleand up." % (derivation))

                # If anything else, pass
                else:
                    self.logger.info("Asynchronous derivation <%s> has unhandled status type: %s." % (
                        derivation, statusType))
                    pass

        else:
            self.logger.info("Currently, no asynchronous derivation <isDerivedUsing> <%s>." % (self.agentIRI))

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        """
        This method perform the agent logic of converting derivation inputs to derivation outputs.
        Developer shall override this when writing new derivation agent based on DerivationAgent class.

        Arguments
            - derivation_inputs: instance of derivation inputs in the format of:
            {
                "https://www.example.com/triplestore/repository/Ontology.owl#Concept_1": ["https://www.example.com/triplestore/repository/Concept_1/Instance_1"],
                "https://www.example.com/triplestore/repository/Ontology.owl#Concept_2": ["https://www.example.com/triplestore/repository/Concept_2/Instance_2"],
                "https://www.example.com/triplestore/repository/Ontology.owl#Concept_3":
                ["https://www.example.com/triplestore/repository/Concept_3/Instance_3_1",
                    "https://www.example.com/triplestore/repository/Concept_3/Instance_3_2"],
                "https://www.example.com/triplestore/repository/Ontology.owl#Concept_4": ["https://www.example.com/triplestore/repository/Concept_4/Instance_4"]
            }
            - derivation_outputs: instance of derivation outputs, developer should add new created entiteis and triples to this variable
        """
        pass

    def add_job_monitoring_derivations(self):
        """
            This method schedules the periodical job to monitor asynchronous derivation, also adds the HTTP endpoint to handle synchronous derivation.
        """
        self.scheduler.add_job(id='monitor_derivations', func=self.monitor_async_derivations,
                               trigger='interval', seconds=self.time_interval)
        self.logger.info("Monitor asynchronous derivations job is scheduled with a time interval of %d seconds." % (
            self.time_interval))

        self.add_url_pattern(self.agentEndpoint, self.agentEndpoint[1:], self.handle_sync_derivations, methods=['GET'])
        self.logger.info("Synchronous derivations can be handled at endpoint: " + self.agentEndpoint)

    def start(self):
        """This method starts all scheduled periodical jobs."""
        if not self.scheduler.running:
            self.scheduler.start()

    def handle_sync_derivations(self):
        requestParams = json.loads(unquote(request.url[len(request.base_url):])[
            len("?query="):])
        res = {}
        if self.validate_inputs(requestParams):
            inputs = self.jpsBaseLib_view.DerivationInputs(requestParams[self.jpsBaseLib_view.DerivationClient.AGENT_INPUT_KEY])
            self.logger.info("Received derivation request parameters: " + str(requestParams))
            derivationIRI = requestParams[self.jpsBaseLib_view.DerivationClient.DERIVATION_KEY]
            derivationType = requestParams[self.jpsBaseLib_view.DerivationClient.DERIVATION_TYPE_KEY]
            outputs = self.jpsBaseLib_view.DerivationOutputs()
            outputs.setThisDerivation(derivationIRI)
            outputs.setRetrievedInputsAt(int(time.time()))
            outputs.setOldEntitiesMap(requestParams[self.jpsBaseLib_view.DerivationClient.BELONGSTO_KEY])
            outputs.setOldEntitiesDownstreamDerivationMap(requestParams[self.jpsBaseLib_view.DerivationClient.DOWNSTREAMDERIVATION_KEY])

            # apply agent logic to convert inputs to outputs
            derivation_inputs = DerivationInputs(inputs)
            derivation_outputs = DerivationOutputs(outputs)
            self.process_request_parameters(derivation_inputs, derivation_outputs)

            derivation = self.jpsBaseLib_view.Derivation(derivationIRI, derivationType)
            if not derivation.isDerivationAsyn() and not derivation.isDerivationWithTimeSeries():
                # construct and fire SPARQL update given DerivationOutputs objects, if normal
                # derivation NOTE this makes sure that the new generated instances/triples will
                # ONLY be written to knowledge graph if the target derivation is till outdated
                # at the point of executing SPARQL update, i.e. this solves concurrent request
                # issue as detailed in
                # https://github.com/cambridge-cares/TheWorldAvatar/issues/184
                triplesChangedForSure = self.derivationClient.reconnectNewDerivedIRIs(
                    outputs.getOutputTriples(), outputs.getNewEntitiesDownstreamDerivationMap(),
                    outputs.getThisDerivation(), outputs.getRetrievedInputsAt()
                )

                # for normal Derivation, we need to return both timestamp and the new derived
                if triplesChangedForSure:
                    # if we know the triples are changed for sure, we return the triples
                    # computed by this agent
                    res[self.jpsBaseLib_view.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY] = outputs.getRetrievedInputsAt()
                    res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_KEY] = json.loads(str(outputs.getNewEntitiesJsonMap()))
                    self.logger.info("Derivation update is done in the knowledge graph, returned response: " + str(res))
                else:
                    # if we are not certain, query the knowledge graph to get the accurate
                    # information
                    updated = self.derivationClient.getDerivation(derivationIRI)
                    res[self.jpsBaseLib_view.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY] = updated.getTimestamp()
                    res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_KEY] = updated.getBelongsToMap()
                    self.logger.info("Unable to determine if the SPARQL update mutated triples, returned latest information in knowledge graph: "
                                     + str(res))
            else:
                # for DerivationWithTimeSeries, we just need to return retrievedInputsAt
                res[self.jpsBaseLib_view.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY] = outputs.getRetrievedInputsAt()
                self.logger.info(
                    "DerivationWithTimeSeries update is done, returned response: " + str(res))
        else:
            res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_KEY] = self.jpsBaseLib_view.DerivationAgent.EMPTY_REQUEST_MSG

        return json.dumps(res)

    def validate_inputs(self, http_request) -> bool:
        return True

    def run_flask_app(self, **kwargs):
        """
            This method runs the flask app as an HTTP servlet.
        """
        self.app.run(**kwargs)
