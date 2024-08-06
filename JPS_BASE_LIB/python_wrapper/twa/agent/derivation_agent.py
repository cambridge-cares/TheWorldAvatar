from abc import ABC, abstractmethod
from typing import Type, TypeVar, Union, List
from flask_apscheduler import APScheduler
from flask import Flask
from flask import request
from flask.typing import RouteCallable
from urllib.parse import urlparse, urljoin
from multiprocessing import Process
import traceback
import yagmail
import json
import time

from twa import agentlogging
from twa.kg_operations import jpsBaseLibGW
from twa.kg_operations import PySparqlClient
from twa.kg_operations.derivation_client import PyDerivationClient
from twa.data_model.base_ontology import BaseClass
from twa.data_model.derivation import DerivationInputs, DerivationOutputs
from twa.data_model.iris import TWA_BASE_URL
from twa.exception import PythonException

# see https://mypy.readthedocs.io/en/latest/generics.html#type-variable-upper-bound
PY_SPARQL_CLIENT = TypeVar('PY_SPARQL_CLIENT', bound=PySparqlClient)


class FlaskConfig(object):
    """
    This class provides the configuration for flask app object.
    Each config should be provided as constant.
    For more information, visit https://flask.palletsprojects.com/en/3.0.x/config/.

    Attributes:
        SCHEDULER_API_ENABLED (bool): Enables the Flask Scheduler API (default is True)
    """
    SCHEDULER_API_ENABLED = True


class DerivationAgent(ABC):
    """
    Abstract base class for a derivation agent.

    This class provides the foundational methods and configurations required for a
    derivation agent. It uses a Flask application and APScheduler to manage asynchronous
    derivations and periodic jobs.

    Attributes:
        app (Flask): The Flask application instance.
        scheduler (APScheduler): The APScheduler instance for managing periodic jobs.
        time_interval (int): The time interval between two runs of the derivation monitoring job (in seconds).
        max_thread_monitor_async_derivations (int): Maximum number of threads to be used for monitoring async derivations.
        syncDerivationEndpoint (str): HTTP endpoint for handling synchronous derivations.
        kgUrl (str): SPARQL query endpoint.
        kgUpdateUrl (str): SPARQL update endpoint.
        kgUser (str, optional): Username for the SPARQL endpoints.
        kgPassword (str, optional): Password for the SPARQL endpoints.
        fs_url (str, optional): File server endpoint.
        fs_user (str, optional): Username for the file server.
        fs_password (str, optional): Password for the file server.
        derivation_client (PyDerivationClient): Client for managing derivations.
        sparql_client (PySparqlClient, optional): SPARQL client instance.
        logger (Logger): Logger for the agent.
        yag (yagmail.SMTP, optional): Email client for sending notifications.
        email_recipient (List[str], optional): List of email recipients.
        email_subject_prefix (str, optional): Prefix for email subjects.
        email_start_end_async_derivations (bool): Flag to send email notifications at the start and end of async derivations.
        register_agent (bool): Flag to register the agent to the knowledge graph.
    """

    def __init__(
        self,
        time_interval: int,
        kg_url: str,
        kg_update_url: str = None,
        kg_user: str = None,
        kg_password: str = None,
        fs_url: str = None,
        fs_user: str = None,
        fs_password: str = None,
        derivation_instance_base_url: str = TWA_BASE_URL,
        flask_config: FlaskConfig = FlaskConfig(),
        agent_endpoint_base_url: str = "http://localhost:5000/",
        register_agent: bool = True,
        max_thread_monitor_async_derivations: int = 1,
        email_recipient: str = '',
        email_subject_prefix: str = '',
        email_username: str = '',
        email_auth_json_path: str = '',
        email_start_end_async_derivations: bool = False,
        logger_for_dev: bool = True,
    ):
        """
        Initialises the instance of DerivationAgent.

        Args:
            time_interval (int): time interval between two runs of derivation monitoring job (in SECONDS)
            kg_url (str): SPARQL query endpoint, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
            kg_update_url (str): SPARQL update endpoint, will be set to the same value as kg_url if not provided, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
            kg_user (str): username used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
            kg_password (str): password that set for the kg_user used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
            fs_url (str): file server endpoint, an example: "http://localhost:8080/FileServer/"
            fs_user (str): username used to access the file server endpoint specified by fs_url
            fs_password (str): password that set for the fs_user used to access the file server endpoint specified by fs_url
            derivation_instance_base_url (str): namespace to be used when creating derivation instance, an example: "http://example.com/kg/"
            flask_config (FlaskConfig): configuration object for flask app, should be an instance of the class FlaskConfig provided as part of this package
            agent_endpoint_base_url (str): data property OntoAgent:hasHttpUrl of OntoAgent:Operation of the derivation agent, an example: "http://localhost:5000/endpoint"
            register_agent (bool): boolean value, whether to register the agent to the knowledge graph
            max_thread_monitor_async_derivations (int): maximum number of threads that to be used for monitoring async derivations
            email_recipient (str): recipients of email notification seperated by semicolon, e.g. "abc@email.com;def@email.com"
            email_subject_prefix (str): subject prefix of the email title to put in a square bracket, e.g. the email subject will start with "[My Prefix]" when provided "My Prefix"
            email_username (str): the username to be used as the sender of the email
            email_auth_json_path (str): file path to the auth json for the `email_username`
            email_start_end_async_derivations (bool): a boolean flag indicating whether to send email notification at the start and end of processing async derivations
            logger_for_dev (bool): logger for agents in development or production
        """

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.agent.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.derivation.*")

        # initialise flask app with its configuration
        self.app = Flask(self.__class__.__name__)
        self.app.config.from_object(flask_config)

        # initialise flask scheduler and assign time interval for monitor_async_derivations job
        self.scheduler = APScheduler(app=self.app)
        self.time_interval = time_interval
        self.max_thread_monitor_async_derivations = max_thread_monitor_async_derivations

        # assign IRI and HTTP URL of the agent
        # self.agentIRI
        self.syncDerivationEndpoint = agent_endpoint_base_url + 'derivation' if agent_endpoint_base_url.endswith('/') else agent_endpoint_base_url + '/derivation'

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
        self.logger = agentlogging.get_logger('dev' if logger_for_dev else 'prod')

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
                "Failed to register the agent <{}> to the KG <{}>. Error: {}".format(self.__class__.agentIRI, self.kgUrl, e),
                stack_info=True, exc_info=True)
            raise e

        self.logger.info(
            "DerivationAgent <%s> is initialised to monitor derivations in triple store <%s> with a time interval of %d seconds." % (
                self.__class__.agentIRI, self.kgUrl, self.time_interval)
        )

    @classmethod
    @property
    def agentIRI(cls):
        return urljoin(TWA_BASE_URL, cls.__name__)

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
        """Decorator to send an email when an exception occurs in the decorated method."""
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

    def send_email(self, subject: str, contents: list):
        """
        Sends an email notification with the given subject and contents.

        Args:
            subject (str): The subject of the email
            contents (list): The contents of the email
        """
        timeout = 2
        process_email = Process(target=self.yag.send, args=(self.email_recipient, subject, contents))
        process_email.start()
        process_email.join(timeout=timeout)
        if process_email.is_alive():
            process_email.kill()
            process_email.join()
        if process_email.exitcode != 0:
            self.logger.error(f"Timed out sending email notification after {timeout} seconds.\n Recipient: {self.email_recipient}\n Subject: {subject}\n Contents: {contents}")

    def send_email_when_async_derivation_up_to_date(self, derivation_iri: str):
        """
        Sends an email notification when an asynchronous derivation is up-to-date.

        Args:
            derivation_iri (str): The IRI of the derivation that was processed
        """
        if self.yag is not None and self.email_start_end_async_derivations:
            self.send_email(
                f"[{self.email_subject_prefix}] async derivation up-to-date",
                [format_current_time(), f"{derivation_iri}"]
            )

    def send_email_when_async_derivation_started(self, derivation_iri):
        """
        Sends an email notification when an asynchronous derivation starts.

        Args:
            derivation_iri (str): The IRI of the derivation that was processed
        """
        if self.yag is not None and self.email_start_end_async_derivations:
            self.send_email(
                f"[{self.email_subject_prefix}] async derivation now-in-progress",
                [format_current_time(), f"{derivation_iri}"]
            )

    def get_sparql_client(self, sparql_client_cls: Type[PY_SPARQL_CLIENT]) -> PY_SPARQL_CLIENT:
        """
        Returns a SPARQL client object that instantiated from sparql_client_cls, which should extend PySparqlClient class.

        Args:
            sparql_client_cls (Type[PY_SPARQL_CLIENT]): The SPARQL client class

        Returns:
            PY_SPARQL_CLIENT: An instance of the SPARQL client
        """
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
            input_concepts = self.agent_input_concepts
            output_concepts = self.agent_output_concepts
            if not isinstance(input_concepts, list) or not isinstance(output_concepts, list):
                raise Exception("Failed to proceed with registering the agent <{}> to the KG <{}>. Error: Input and output concepts must be lists. Received: {} (type: {}) and {} (type: {})".format(
                    self.__class__.agentIRI, self.kgUrl, input_concepts, type(input_concepts), output_concepts, type(output_concepts)))
            if len(input_concepts) == 0 or len(output_concepts) == 0:
                raise Exception("Failed to proceed with registering the agent <{}> to the KG <{}>. Error: No input or output concepts specified.".format(self.__class__.agentIRI, self.kgUrl))
            input_concepts_iris = [o if isinstance(o, str) else o.rdf_type for o in input_concepts]
            output_concepts_iris = [o if isinstance(o, str) else o.rdf_type for o in output_concepts]
            self.derivation_client.createOntoAgentInstance(self.__class__.agentIRI, self.syncDerivationEndpoint, input_concepts_iris, output_concepts_iris)
            self.logger.info("Agent <%s> is registered to the KG <%s> with input signature %s and output signature %s." % (
                self.__class__.agentIRI, self.kgUrl, input_concepts, output_concepts))
        else:
            self.logger.info("Flag register_agent is False. Agent <%s> is NOT registered to the KG <%s>." % (self.__class__.agentIRI, self.kgUrl))

    @property
    @classmethod
    @abstractmethod
    def agent_input_concepts(cls) -> List[Union[str, BaseClass]]:
        """This method returns a list of input concepts of the agent. This should be overridden by the derived class."""
        pass

    @property
    @classmethod
    @abstractmethod
    def agent_output_concepts(cls) -> List[Union[str, BaseClass]]:
        """This method returns a list of output concepts of the agent. This should be overridden by the derived class."""
        pass

    def add_url_pattern(
        self,
        url_pattern: str = None,
        url_pattern_name: str = None,
        function: RouteCallable = None,
        methods: str = ['GET'],
        *args,
        **kwargs
    ):
        """
        This method is a wrapper of add_url_rule method of Flask object that adds customised URL Pattern to derivation agent.
        For more information, visit https://flask.palletsprojects.com/en/3.0.x/api/#flask.Flask.add_url_rule
        WARNING: Use of this by developer is STRONGLY discouraged.
        The design intention of an derivation agent is to communicate via the KNOWLEDGE GRAPH, and NOT via HTTP requests.

        Args:
            url_pattern (str): the endpoint url to associate with the rule and view function
            url_pattern_name (str): the name of the endpoint
            function (flask.typing.RouteCallable): the view function to associate with the endpoint
            methods (str): HTTP request methods, default to ['GET']
        """
        self.app.add_url_rule(url_pattern, url_pattern_name,
                              function, methods=methods, *args, **kwargs)
        self.logger.info(f"A URL Pattern <{url_pattern}> is added.")

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

        Args:
            derivation_inputs (DerivationInputs): instance of derivation inputs, essentially in the format of:
                {
                    "https://example.com/kg/Concept_1": ["https://example.com/kg/Concept_1/Instance_1"],
                    "https://example.com/kg/Concept_2": ["https://example.com/kg/Concept_2/Instance_2"],
                    "https://example.com/kg/Concept_3":
                    ["https://example.com/kg/Concept_3/Instance_3_1",
                        "https://example.com/kg/Concept_3/Instance_3_2"],
                    "https://example.com/kg/Concept_4": ["https://example.com/kg/Concept_4/Instance_4"]
                }
            derivation_outputs (DerivationOutputs): instance of derivation outputs, developer should add new created entiteis and triples to this variable
        """
        pass

    @periodical_job
    def _start_monitoring_derivations(self):
        """This method starts the periodical job to monitor asynchronous derivation, also adds the HTTP endpoint to handle synchronous derivation."""
        self.scheduler.add_job(id='monitor_derivations', func=self.monitor_async_derivations,
                               trigger='interval', seconds=self.time_interval,
                               max_instances=self.max_thread_monitor_async_derivations) # enable multiple threads if needed, default is 1
        self.logger.info("Monitor asynchronous derivations job is scheduled with a time interval of %d seconds." % (
            self.time_interval))

        url_pattern_name = f'{self.__class__.__name__}_handle_sync_derivations'
        self.add_url_pattern(urlparse(self.syncDerivationEndpoint).path, url_pattern_name, self.handle_sync_derivations, methods=['POST'])
        self.logger.info("Synchronous derivations can be handled at endpoint: " + self.syncDerivationEndpoint)

    def start_all_periodical_job(self):
        """This method starts all scheduled periodical jobs."""
        all_periodical_jobs = [getattr(self, name) for name in dir(self) if callable(getattr(self, name)) and not name.startswith('__') and hasattr(getattr(self, name), '__is_periodical_job__')]
        for func in all_periodical_jobs:
            func()

    @send_email_when_exception(func_return_value=True)
    def handle_sync_derivations(self):
        """
        This method handles synchronous derivation by using the Flask app object of the DerivationAgent to process the HTTP request,
        and then pass it to `process_request_parameters` function provided by the developers.
        """
        self.logger.info("Received synchronous derivation request: %s." % (request.url))
        requestParams = request.json
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
        """
        Validates the HTTP request sent to the agent for processing synchronous derivations.
        Developer can overwrite this function for customised validation.

        Args:
            http_request: The HTTP request received

        Raises:
            Exception: HTTP request is empty
            Exception: IRI for derivation is not provided in the HTTP request
            Exception: IRI for agent is not provided in the HTTP request
            Exception: IRI for old derivation outputs are not provided in the HTTP request
            Exception: IRI for downstream derivations are not provided in the HTTP request

        Returns:
            bool: Whether the HTTP request is valid
        """
        self.logger.info("Validating inputs: " + str(http_request))
        if not bool(http_request):
            self.logger.warn("RequestParams are empty, throwing BadRequestException...")
            raise Exception("RequestParams are empty")

        if self.jpsBaseLib_view.DerivationClient.AGENT_INPUT_KEY not in http_request:
            self.logger.info(f"Agent <{self.agentIRI}> received an empty request...")
            return False
        else:
            if self.jpsBaseLib_view.DerivationClient.DERIVATION_KEY not in http_request:
                msg = f"Agent <{self.agentIRI}> received a request that doesn't have derivationIRI..."
                self.logger.error(msg)
                raise Exception(msg)
            if http_request[self.jpsBaseLib_view.DerivationClient.SYNC_NEW_INFO_FLAG]:
                if self.jpsBaseLib_view.DerivationClient.AGENT_IRI_KEY not in http_request:
                    msg = f"Agent <{self.agentIRI}> received a request for sync new information that doesn't have information about agent IRI..."
                    self.logger.error(msg)
                    raise Exception(msg)
            else:
                if self.jpsBaseLib_view.DerivationClient.BELONGSTO_KEY not in http_request:
                    msg = f"Agent <{self.agentIRI}> received a request that doesn't have information about old outputs..."
                    self.logger.error(msg)
                    raise Exception(msg)
                if self.jpsBaseLib_view.DerivationClient.DOWNSTREAMDERIVATION_KEY not in http_request:
                    msg = f"Agent <{self.agentIRI}> received a request that doesn't have information about downstream derivation..."
                    self.logger.error(msg)
                    raise Exception(msg)

        return True

    def run_flask_app(self, **kwargs):
        """This method runs the flask app as an HTTP servlet."""
        self.app.run(**kwargs)

def format_current_time() -> str:
    """
    Formats the current local time as a string.

    Returns:
        str: The current local time in the format "YYYY-MM-DD HH:MM:SS TIMEZONE".
    """
    return str(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())) + f" {str(time.localtime().tm_zone)}"
