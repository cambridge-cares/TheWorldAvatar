from abc import ABC, abstractmethod
from typing import Type, TypeVar
from flask_apscheduler import APScheduler
from flask import Flask
from flask import request
from urllib.parse import unquote
from urllib.parse import urlparse
from multiprocessing import Process
import traceback
import yagmail
import json
import time

from py4jps import agentlogging

from pyderivationagent.kg_operations import jpsBaseLibGW
from pyderivationagent.kg_operations import PySparqlClient
from pyderivationagent.kg_operations import PyDerivationClient
from pyderivationagent.data_model import DerivationInputs, DerivationOutputs
from pyderivationagent.exception import PythonException

# see https://mypy.readthedocs.io/en/latest/generics.html#type-variable-upper-bound
PY_SPARQL_CLIENT = TypeVar('PY_SPARQL_CLIENT', bound=PySparqlClient)


class FlaskConfig(object):
    """
        This class provides the configuration for flask app object. Each config should be provided as constant. For more information, visit https://flask.palletsprojects.com/en/2.0.x/config/.
    """
    SCHEDULER_API_ENABLED = True


class DerivationAgent(ABC):
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
        agent_endpoint: str = "http://localhost:5000/sync_derivation",
        register_agent: bool = True,
        max_thread_monitor_async_derivations: int = 1,
        email_recipient: str = '',
        email_subject_prefix: str = '',
        email_username: str = '',
        email_auth_json_path: str = '',
        email_start_end_async_derivations: bool = False,
        logger_name: str = "dev"
    ):
        """
            This method initialises the instance of DerivationAgent.

            Arguments:
                app - flask app object, an example: app = Flask(__name__)
                agent_iri - OntoAgent:Service IRI of the derivation agent, an example: "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
                agent_endpoint - data property OntoAgent:hasHttpUrl of OntoAgent:Operation of the derivation agent, an example: "http://localhost:5000/endpoint"
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
                register_agent - boolean value, whether to register the agent to the knowledge graph
                logger_name - logger names for getting correct loggers from agentlogging package, valid logger names: "dev" and "prod", for more information, visit https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/python_wrapper/py4jps/agentlogging/logging.py
        """

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.agent.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.derivation.*")

        # initialise flask app with its configuration
        self.app = app
        self.app.config.from_object(flask_config)

        # initialise flask scheduler and assign time interval for monitor_async_derivations job
        self.scheduler = APScheduler(app=self.app)
        self.time_interval = time_interval
        self.max_thread_monitor_async_derivations = max_thread_monitor_async_derivations

        # assign IRI and HTTP URL of the agent
        self.agentIRI = agent_iri
        self.agentEndpoint = agent_endpoint

        # assign KG related information
        self.kgUrl = kg_url
        self.kgUpdateUrl = kg_update_url if kg_update_url is not None else kg_url
        # NOTE that we check first if below are empty string first
        # as the config_derivation_agent will read as '' if the value is not provided in env file
        self.kgUser = kg_user if kg_user != '' else None
        self.kgPassword = kg_password if kg_password != '' else None

        # assign file server related information
        # NOTE that we check first if below are empty string first
        # as the config_derivation_agent will read as '' if the value is not provided in env file
        self.fs_url = fs_url if fs_url != '' else None
        self.fs_user = fs_user if fs_user != '' else None
        self.fs_password = fs_password if fs_password != '' else None

        # initialise the derivation_client with SPARQL Query and Update endpoint
        self.derivation_client = PyDerivationClient(
            derivation_instance_base_url,
            self.kgUrl,
            self.kgUpdateUrl,
            self.kgUser,
            self.kgPassword,
        )

        # initialise the SPARQL client as None, this will be replaced when get_sparql_client() is first called
        self.sparql_client = None

        # initialise the logger
        self.logger = agentlogging.get_logger(logger_name)

        # initialise the email object and email_start_end_async_derivations flag
        if all([bool(param) for param in [email_recipient, email_username, email_auth_json_path]]):
            self.yag = yagmail.SMTP(email_username, oauth2_file=email_auth_json_path)
            self.email_recipient = email_recipient.split(';')
            self.email_subject_prefix = email_subject_prefix if bool(email_subject_prefix) else str(self.__class__.__name__)
        else:
            self.yag = None
        self.email_start_end_async_derivations = email_start_end_async_derivations

        # register the agent to the KG if required
        self.register_agent = register_agent
        try:
            self.register_agent_in_kg()
        except Exception as e:
            self.logger.error(
                "Failed to register the agent <{}> to the KG <{}>. Error: {}".format(self.agentIRI, self.kgUrl, e),
                stack_info=True, exc_info=True)
            raise e

        self.logger.info(
            "DerivationAgent <%s> is initialised to monitor derivations in triple store <%s> with a time interval of %d seconds." % (
                self.agentIRI, self.kgUrl, self.time_interval)
        )

    def periodical_job(func):
        """This method is used to start a periodic job. This should be used as a decorator (@Derivation.periodical_job) for the method that needs to be executed periodically."""
        def inner(self, *args, **kwargs):
            func(self, *args, **kwargs)
            if not self.scheduler.running:
                self.scheduler.start()
                self.logger.info("Scheduler is started.")
        inner.__is_periodical_job__ = True
        return inner

    def send_email_when_exception(func_return_value=False):
        def decorator(func):
            def inner(self, *args, **kwargs):
                try:
                    if not func_return_value:
                        func(self, *args, **kwargs)
                    else:
                        return func(self, *args, **kwargs)
                except Exception as e:
                    if self.yag is not None:
                        self.send_email(
                            f"[{self.email_subject_prefix}] exception: {str(func.__name__)}",
                            [
                                format_current_time(),
                                # the "<" and ">" may exist in exception message if any IRI is presented, e.g. <http://iri>
                                # here we replace them to HTML entities, so that the IRIs can be displayed correctly
                                # for more information about HTML entities, visit https://www.w3schools.com/html/html_entities.asp
                                str(e).replace("<", "&lt;").replace(">", "&gt;"),
                                traceback.format_exc()
                            ]
                        )
                    # Log error regardless
                    self.logger.exception(e)
            return inner
        return decorator

    def send_email(self, subject, contents):
        timeout = 2
        process_email = Process(target=self.yag.send, args=(self.email_recipient, subject, contents))
        process_email.start()
        process_email.join(timeout=timeout)
        if process_email.is_alive():
            process_email.kill()
            process_email.join()
        if process_email.exitcode != 0:
            self.logger.error(f"Timed out sending email notification after {timeout} seconds.\n Recipient: {self.email_recipient}\n Subject: {subject}\n Contents: {contents}")

    def send_email_when_async_derivation_up_to_date(self, derivation_iri):
        if self.yag is not None and self.email_start_end_async_derivations:
            self.send_email(
                f"[{self.email_subject_prefix}] async derivation up-to-date",
                [format_current_time(), f"{derivation_iri}"]
            )

    def send_email_when_async_derivation_started(self, derivation_iri):
        if self.yag is not None and self.email_start_end_async_derivations:
            self.send_email(
                f"[{self.email_subject_prefix}] async derivation now-in-progress",
                [format_current_time(), f"{derivation_iri}"]
            )

    def get_sparql_client(self, sparql_client_cls: Type[PY_SPARQL_CLIENT]) -> PY_SPARQL_CLIENT:
        """This method returns a SPARQL client object that instantiated from sparql_client_cls, which should extend PySparqlClient class."""
        if self.sparql_client is None or not isinstance(self.sparql_client, sparql_client_cls):
            self.sparql_client = sparql_client_cls(
                query_endpoint=self.kgUrl, update_endpoint=self.kgUpdateUrl,
                kg_user=self.kgUser, kg_password=self.kgPassword,
                fs_url=self.fs_url, fs_user=self.fs_user, fs_pwd=self.fs_password
            )
        return self.sparql_client

    def register_agent_in_kg(self):
        """This method registers the agent to the knowledge graph by uploading its OntoAgent triples generated on-the-fly."""
        if self.register_agent:
            input_concepts = self.agent_input_concepts()
            output_concepts = self.agent_output_concepts()
            if not isinstance(input_concepts, list) or not isinstance(output_concepts, list):
                raise Exception("Failed to proceed with registering the agent <{}> to the KG <{}>. Error: Input and output concepts must be lists. Received: {} (type: {}) and {} (type: {})".format(
                    self.agentIRI, self.kgUrl, input_concepts, type(input_concepts), output_concepts, type(output_concepts)))
            if len(input_concepts) == 0 or len(output_concepts) == 0:
                raise Exception("Failed to proceed with registering the agent <{}> to the KG <{}>. Error: No input or output concepts specified.".format(self.agentIRI, self.kgUrl))
            self.derivation_client.createOntoAgentInstance(self.agentIRI, self.agentEndpoint, input_concepts, output_concepts)
            self.logger.info("Agent <%s> is registered to the KG <%s> with input signature %s and output signature %s." % (
                self.agentIRI, self.kgUrl, input_concepts, output_concepts))
        else:
            self.logger.info("Flag register_agent is False. Agent <%s> is NOT registered to the KG <%s>." % (self.agentIRI, self.kgUrl))

    @abstractmethod
    def agent_input_concepts(self) -> list:
        """This method returns a list of input concepts of the agent. This should be overridden by the derived class."""
        pass

    @abstractmethod
    def agent_output_concepts(self) -> list:
        """This method returns a list of output concepts of the agent. This should be overridden by the derived class."""
        pass

    def add_url_pattern(self, url_pattern=None, url_pattern_name=None, function=None, methods=['GET'], *args, **kwargs):
        """
            This method is a wrapper of add_url_rule method of Flask object that adds customised URL Pattern to derivation agent.
            For more information, visit https://flask.palletsprojects.com/en/2.0.x/api/#flask.Flask.add_url_rule
            WARNING: Use of this by developer is STRONGLY discouraged.
            The design intention of an derivation agent is to communicate via the KNOWLEDGE GRAPH, and NOT via HTTP requests.

            Arguments:
                url_pattern - the endpoint url to associate with the rule and view function
                url_pattern_name - the name of the endpoint
                function - the view function to associate with the endpoint
                methods - HTTP request methods, default to ['GET']
        """
        self.app.add_url_rule(url_pattern, url_pattern_name,
                              function, methods=methods, *args, **kwargs)
        self.logger.info("A URL Pattern <%s> is added." % (url_pattern))

    @send_email_when_exception(func_return_value=False)
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
        # for more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/agent/DerivationAgent.java

        # Initialise two conditions for the while loop
        # 1. break_out_time is the timestamp when the next round of monitoring should be started
        break_out_time = time.time() + self.time_interval
        # 2. query_again is the flag to indicate whether the derivation status should be updated in memory
        query_again = False

        # There is no do-while loop in Python, so we use a while loop with a break statement
        # "while True" makes sure the loop is executed at least once
        while True:
            # Retrieves a list of derivations and their status type that "isDerivedUsing" DerivationAgent
            derivationAndStatusType = self.derivation_client.derivation_client.getDerivationsAndStatusType(
                self.agentIRI)
            if bool(derivationAndStatusType):
                self.logger.info("A list of asynchronous derivations that <isDerivedUsing> <%s> are retrieved: %s." % (
                    self.agentIRI, {d: str(derivationAndStatusType[d]) for d in derivationAndStatusType}))

                # Iterate over the list of derivation, and do different things depend on the derivation status
                for derivation in derivationAndStatusType:
                    statusType = str(derivationAndStatusType[derivation])

                    try:
                        # If "Requested", check the immediate upstream derivations if they are up-to-date
                        # if any of the asynchronous derivations are still outdated, skip, otherwise, request update of all synchronous derivations
                        # then retrieve inputs, marks as "InProgress", start job, update status at job completion
                        if statusType == 'REQUESTED':
                            immediateUpstreamDerivationToUpdate = self.derivation_client.derivation_client.checkImmediateUpstreamDerivation(derivation)
                            if self.jpsBaseLib_view.DerivationSparql.ONTODERIVATION_DERIVATIONASYN in immediateUpstreamDerivationToUpdate:
                                self.logger.info("Asynchronous derivation <" + derivation
                                                + "> has a list of immediate upstream asynchronous derivations to be updated: "
                                                + str(immediateUpstreamDerivationToUpdate))
                                # set flag to false to skips this "Requested" derivation until next time
                                # this is to avoid the agent flooding the KG with queries of the status over a short period of time
                                query_again = False
                            else:
                                syncDerivationsToUpdate = self.derivation_client.derivation_client.groupSyncDerivationsToUpdate(immediateUpstreamDerivationToUpdate)
                                if bool(syncDerivationsToUpdate):
                                    self.logger.info("Asynchronous derivation <" + derivation
                                                    + "> has a list of immediate upstream synchronous derivations to be updated: "
                                                    + str(syncDerivationsToUpdate))
                                    self.derivation_client.derivation_client.updatePureSyncDerivations(syncDerivationsToUpdate)
                                    self.logger.info("Update of synchronous derivation is done for: " + str(syncDerivationsToUpdate))
                                if not bool(self.derivation_client.derivation_client.checkImmediateUpstreamDerivation(derivation)):
                                    agentInputs = str(self.derivation_client.derivation_client.retrieveAgentInputIRIs(derivation, self.agentIRI))
                                    # Mark the status as "InProgress"
                                    # if another agent thread is updating the same derivation concurrently
                                    # and successed before this thread, then this method will return false
                                    progressToJob = self.derivation_client.derivation_client.updateStatusBeforeSetupJob(derivation)
                                    # only progress to job if the status is updated successfully
                                    # otherwise, the other thread will handle the job
                                    if not progressToJob:
                                        self.logger.info(f"Asynchronous derivation <{derivation}> is already in progress by another agent thread.")
                                    else:
                                        self.logger.info("Agent <%s> retrieved inputs of asynchronous derivation <%s>: %s." % (
                                            self.agentIRI, derivation, agentInputs))
                                        self.logger.info("Asynchronous derivation <%s> is in progress." % (derivation))
                                        # send email to indicate the derivation is handled in this thread and started, i.e. now-in-progress
                                        self.send_email_when_async_derivation_started(derivation)

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
                                        derivationInputs = self.jpsBaseLib_view.DerivationInputs(inputs_to_send, derivation)
                                        derivation_inputs = DerivationInputs(derivationInputs)
                                        derivationOutputs = self.jpsBaseLib_view.DerivationOutputs()
                                        derivation_outputs = DerivationOutputs(derivationOutputs)
                                        self.process_request_parameters(derivation_inputs, derivation_outputs)

                                        newDerivedIRI = derivationOutputs.getNewDerivedIRI()
                                        newTriples = derivationOutputs.getOutputTriples()
                                        self.derivation_client.derivation_client.updateStatusAtJobCompletion(derivation, newDerivedIRI, newTriples)
                                        self.logger.info("Asynchronous derivation <%s> generated new derived IRI: <%s>." % (
                                            derivation, ">, <".join(newDerivedIRI)))
                                        self.logger.info("Asynchronous derivation <" + derivation +
                                                        "> has all new generated triples: " + str([t.getQueryString() for t in newTriples]))
                                        self.logger.info("Asynchronous derivation <" + derivation + "> is now finished, to be cleaned up.")

                                # set flag to true as either (1) the agent has been process this derivation for some time
                                # and status of other derivations in KG might have changed by other processes during this time
                                # or (2) the derivation is processed by another agent therefore needs a record update
                                query_again = True

                        # If "InProgress", pass
                        elif statusType == 'INPROGRESS':
                            # the query_again flag is set as false to let agent carry on to next derivation in the list
                            query_again = False

                        # If "Finished", do all the clean-up steps
                        elif statusType == 'FINISHED':
                            self.derivation_client.derivation_client.cleanUpFinishedDerivationUpdate(derivation)
                            # set flag to true as the cleaning up process can take some time when there are a lot of triples
                            query_again = True
                            # send email to indicate the derivation is now finished and cleaned up, i.e. up-to-date
                            self.send_email_when_async_derivation_up_to_date(derivation)

                        elif statusType == 'ERROR':
                            # for now just passes
                            query_again = False

                        elif statusType == 'NOSTATUS':
                            # no need to query_again as the derivation is considered as up-to-date
                            query_again = False

                        # If anything else, pass
                        else:
                            self.logger.info("Asynchronous derivation <%s> has unhandled status type: %s." % (
                                derivation, statusType))
                            query_again = False

                    except Exception as exc:
                        trace_back = traceback.format_exc()
                        jps_exc = PythonException(trace_back)
                        self.derivation_client.derivation_client.markAsError(derivation, jps_exc.exception)
                        query_again = True
                        self.logger.error(f"Error when handling derivation <{derivation}>", stack_info=True, exc_info=True)
                        # Raise exception so that this will be sent via email notification
                        raise exc

                    # Break out the for loop and query again the list of derivations and their status
                    if query_again:
                        break

            else:
                self.logger.info("Currently, no asynchronous derivation <isDerivedUsing> <%s>." % (self.agentIRI))

            # Check if the two flags are still met, if not, break out the while loop
            # i.e. process until the time is up and if have not gone through all derivations
            if time.time() > break_out_time or not query_again:
                break

    @abstractmethod
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

    @periodical_job
    def _start_monitoring_derivations(self):
        """
            This method starts the periodical job to monitor asynchronous derivation, also adds the HTTP endpoint to handle synchronous derivation.
        """
        self.scheduler.add_job(id='monitor_derivations', func=self.monitor_async_derivations,
                               trigger='interval', seconds=self.time_interval,
                               max_instances=self.max_thread_monitor_async_derivations) # enable multiple threads if needed, default is 1
        self.logger.info("Monitor asynchronous derivations job is scheduled with a time interval of %d seconds." % (
            self.time_interval))

        url_pattern = urlparse(self.agentEndpoint).path
        url_pattern_name = url_pattern.strip('/').replace('/', '_') + '_handle_sync_derivations'
        self.add_url_pattern(url_pattern, url_pattern_name, self.handle_sync_derivations, methods=['GET'])
        self.logger.info("Synchronous derivations can be handled at endpoint: " + self.agentEndpoint)

    def start_all_periodical_job(self):
        """This method starts all scheduled periodical jobs."""
        all_periodical_jobs = [getattr(self, name) for name in dir(self) if callable(getattr(self, name)) and not name.startswith('__') and hasattr(getattr(self, name), '__is_periodical_job__')]
        for func in all_periodical_jobs:
            func()

    @send_email_when_exception(func_return_value=True)
    def handle_sync_derivations(self):
        self.logger.info("Received synchronous derivation request: %s." % (request.url))
        requestParams = json.loads(unquote(urlparse(request.url).query)[len("query="):])
        res = {}
        if self.validate_inputs(requestParams):
            # retrieve necessary information
            derivationIRI = requestParams[self.jpsBaseLib_view.DerivationClient.DERIVATION_KEY]
            derivationType = requestParams[self.jpsBaseLib_view.DerivationClient.DERIVATION_TYPE_KEY]
            syncNewInfoFlag = requestParams[self.jpsBaseLib_view.DerivationClient.SYNC_NEW_INFO_FLAG]

            # serialises DerivationInputs objects from JSONObject
            inputs = self.jpsBaseLib_view.DerivationInputs(requestParams[self.jpsBaseLib_view.DerivationClient.AGENT_INPUT_KEY], derivationIRI)
            self.logger.info("Received derivation request parameters: " + str(requestParams))

            # initialise DerivationOutputs, also set up information
            outputs = self.jpsBaseLib_view.DerivationOutputs()
            outputs.setThisDerivation(derivationIRI)
            outputs.setRetrievedInputsAt(int(time.time()))
            if not syncNewInfoFlag:
                outputs.setOldEntitiesMap(requestParams[self.jpsBaseLib_view.DerivationClient.BELONGSTO_KEY])
                outputs.setOldEntitiesDownstreamDerivationMap(requestParams[self.jpsBaseLib_view.DerivationClient.DOWNSTREAMDERIVATION_KEY])

            # apply agent logic to convert inputs to outputs
            derivation_inputs = DerivationInputs(inputs)
            derivation_outputs = DerivationOutputs(outputs)
            self.process_request_parameters(derivation_inputs, derivation_outputs)

            # return response if this sync derivation is generated for new info
            if syncNewInfoFlag:
                agentServiceIRI = requestParams[self.jpsBaseLib_view.DerivationClient.AGENT_IRI_KEY]
                self.derivation_client.derivation_client.writeSyncDerivationNewInfo(
                    outputs.getOutputTriples(), outputs.getNewDerivedIRI(),
                    agentServiceIRI, inputs.getAllIris(),
                    derivationIRI, derivationType, outputs.getRetrievedInputsAt()
                )
                res[self.jpsBaseLib_view.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY] = outputs.getRetrievedInputsAt()
                res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_KEY] = json.loads(str(outputs.getNewEntitiesJsonMap()))
                self.logger.info("Synchronous derivation for new information generated successfully, returned response: " + str(res))
                return json.dumps(res)

            # only enters below if the computation was not for new information (new instances)
            derivation = self.jpsBaseLib_view.Derivation(derivationIRI, derivationType)
            if not derivation.isDerivationAsyn() and not derivation.isDerivationWithTimeSeries():
                # Perform the mapping between the new outputs and the downstream derivations
                connectionMap = self.derivation_client.derivation_client.mapSyncNewOutputsToDownstream(
                    outputs.getThisDerivation(), outputs.getNewOutputsAndRdfTypeMap())
                # construct and fire SPARQL update given DerivationOutputs objects, if normal
                # derivation NOTE this makes sure that the new generated instances/triples will
                # ONLY be written to knowledge graph if the target derivation is till outdated
                # at the point of executing SPARQL update, i.e. this solves concurrent request
                # issue as detailed in
                # https://github.com/cambridge-cares/TheWorldAvatar/issues/184
                triplesChangedForSure = self.derivation_client.derivation_client.reconnectSyncDerivation(
                    outputs.getThisDerivation(), connectionMap,
                    outputs.getOutputTriples(), outputs.getRetrievedInputsAt()
                )

                # for normal Derivation, we need to return both timestamp and the new derived
                if triplesChangedForSure:
                    # if we know the triples are changed for sure, we return the triples
                    # computed by this agent
                    res[self.jpsBaseLib_view.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY] = outputs.getRetrievedInputsAt()
                    res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_KEY] = json.loads(str(outputs.getNewEntitiesJsonMap()))
                    res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_CONNECTION_KEY] = json.loads(str(self.jpsBaseLib_view.org.json.JSONObject(connectionMap)))
                    self.logger.info("Derivation update is done in the knowledge graph, returned response: " + str(res))
                else:
                    # if we are not certain, query the knowledge graph to get the accurate
                    # information
                    updated = self.derivation_client.derivation_client.getDerivation(derivationIRI)
                    res[self.jpsBaseLib_view.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY] = updated.getTimestamp()
                    res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_KEY] = json.loads(str(updated.getBelongsToMap()))
                    res[self.jpsBaseLib_view.DerivationClient.AGENT_OUTPUT_CONNECTION_KEY] = json.loads(str(updated.getDownstreamDerivationConnectionMap()))
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

    @abstractmethod
    def validate_inputs(self, http_request) -> bool:
        # developer can overwrite this function for customised validation
        self.logger.info("Validating inputs: " + str(http_request))
        if not bool(http_request):
            self.logger.warn("RequestParams are empty, throwing BadRequestException...")
            raise Exception("RequestParams are empty")

        if self.jpsBaseLib_view.DerivationClient.AGENT_INPUT_KEY not in http_request:
            self.logger.info("Agent <%s> received an empty request..." % self.agentIRI)
            return False
        else:
            if self.jpsBaseLib_view.DerivationClient.DERIVATION_KEY not in http_request:
                msg = "Agent <%s> received a request that doesn't have derivationIRI..." % self.agentIRI
                self.logger.error(msg)
                raise Exception(msg)
            if http_request[self.jpsBaseLib_view.DerivationClient.SYNC_NEW_INFO_FLAG]:
                if self.jpsBaseLib_view.DerivationClient.AGENT_IRI_KEY not in http_request:
                    msg = "Agent <%s> received a request for sync new information that doesn't have information about agent IRI..." % self.agentIRI
                    self.logger.error(msg)
                    raise Exception(msg)
            else:
                if self.jpsBaseLib_view.DerivationClient.BELONGSTO_KEY not in http_request:
                    msg = "Agent <%s> received a request that doesn't have information about old outputs..." % self.agentIRI
                    self.logger.error(msg)
                    raise Exception(msg)
                if self.jpsBaseLib_view.DerivationClient.DOWNSTREAMDERIVATION_KEY not in http_request:
                    msg = "Agent <%s> received a request that doesn't have information about downstream derivation..." % self.agentIRI
                    self.logger.error(msg)
                    raise Exception(msg)

        return True

    def run_flask_app(self, **kwargs):
        """
            This method runs the flask app as an HTTP servlet.
        """
        self.app.run(**kwargs)

def format_current_time() -> str:
    return str(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())) + f" {str(time.localtime().tm_zone)}"
