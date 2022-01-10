from flask import Flask, jsonify, request
import json
# import agentlogging
from flask_apscheduler import APScheduler
from pyasyncagent.gateway import jpsBaseLibGW

# create a JVM module view and use it to import the required java classes
# TODO should this be part of AsyncAgent class?
# jpsBaseLib_view = jpsBaseLibGW.createModuleView()
# jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
# jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

def async_hello(whom):
    print('Async hello %s!' % whom)

class FlaskConfig(object):
    SCHEDULER_API_ENABLED = True

class AsyncAgent(object):
    def __init__(
        self,
        app: Flask,
        agent_iri: str,
        time_interval: int,
        derivation_instance_base_url: str,
        kg_url: str,
        kg_user: str = None,
        kg_password: str = None,
        # derivationClient,
        flask_config: FlaskConfig = FlaskConfig()):

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


        # agent iri, time interval, derivation base url, kg url, kg user, kg password

        # self.interval_task_id = 'interval-task-id'
        # self.time_interval = 5
        # self.agentIRI = 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service' # for testing purpose, will be provided as part of config
        # self.kgUrl = 'http://kg.cmclinnovations.com:81/blazegraph/namespaces/testontorxn/sparql' # for testing purpose
        # Initialise the derivationClient with SPARQL Query and Update endpoint
        # self.storeClient = jpsBaseLib_view.RemoteStoreClient(self.kgUrl, self.kgUrl)
        # self.derivationClient = jpsBaseLib_view.DerivationClient(self.storeClient)
        # self.derivationClient = derivationClient
    
    # def configs(self, **configs):
    #     for config, value in configs:
    #         self.app.config[config.upper()] = value
    
    def add_url_pattern(self, url_pattern=None, url_pattern_name=None, function=None, methods=['GET'], *args, **kwargs):
        self.app.add_url_rule(url_pattern, url_pattern_name, function, methods=methods, *args, **kwargs)
    
    def monitorDerivations(self):
        # Below codes follow the logic as defined in AsynAgent.java in JPS_BASE_LIB
        # for more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/agent/AsynAgent.java
        derivationAndStatusType = self.derivationClient.getDerivationsAndStatusType(self.agentIRI)

        for derivation in derivationAndStatusType:
            statusType = str(derivationAndStatusType[derivation])
            if statusType == 'PENDINGUPDATE':
                self.derivationClient.checkAtPendingUpdate(derivation)
            elif statusType == 'REQUESTED':
                agentInputs = str(self.derivationClient.retrieveAgentInputs(derivation, self.agentIRI))
                self.derivationClient.markAsInProgress(derivation)
                print(agentInputs)
                print(type(agentInputs))
                newDerivedIRI = self.setupJob(agentInputs)
                self.derivationClient.updateStatusAtJobCompletion(derivation, newDerivedIRI)
            elif statusType == 'INPROGRESS':
                pass
            elif statusType == 'FINISHED':
                self.derivationClient.cleanUpFinishedDerivationUpdate(derivation)
            else:
                pass

        # # Retrieves a list of derivation that "isDerivedUsing" DoE Agent
        # list_of_derivation = self.derivationClient.getDerivations(self.agentIRI)

        # # Iterate over the list of derivation, and do different things depend on the derivation status
        # for derivation in list_of_derivation:
        #     # check if the derivation is an instance of asynchronous derivation
        #     if (self.derivationClient.isDerivedAsynchronous(derivation)):
        #         if (self.derivationClient.isPendingUpdate(derivation)):
        #             self.derivationClient.checkAtPendingUpdate(derivation)
        #         # If "Requested", retrieve inputs, marks as "InProgress", start job, update status at job completion
        #         elif (self.derivationClient.isRequested(derivation)):
        #             agentInputs = self.derivationClient.retrieveAgentInputs(derivation, self.agentIRI)
        #             self.derivationClient.markAsInProgress(derivation)
        #             newDerivedIRI = self.setupjob(json.loads(str(agentInputs)))
        #             self.derivationClient.updateStatusAtJobCompletion(derivation, newDerivedIRI)
        #         # If "InProgress", pass
        #         elif (self.derivationClient.isInProgress(derivation)):
        #             # at the moment the design is the agent just pass when it's detected as "InProgress"
        #             pass
        #         # If "Finished", do all the clean-up steps
        #         elif (self.derivationClient.isFinished(derivation)):
        #             self.derivationClient.cleanUpFinishedDerivationUpdate(derivation)
        #     else:
    	# 		# TODO ideally this should call the update or other functions in synchronous derivation function
    	# 		# LOGGER.info("Derivation instance <" + derivation + "> is not an asynchronous derivation.");
        #         pass
    
    def setupJob(self, agentInputs) -> list:
        createdIRI = []
        return createdIRI

    def run(self, **kwargs):
        self.scheduler.init_app(self.app)
        self.scheduler.add_job(id='monitor_derivations', func=self.monitorDerivations, trigger='interval', seconds=self.time_interval)
        self.scheduler.start()
        self.app.run(**kwargs)
