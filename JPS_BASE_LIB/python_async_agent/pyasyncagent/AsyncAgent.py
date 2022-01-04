from flask import Flask, jsonify, request
import json
# import agentlogging
from flask_apscheduler import APScheduler
# from pyasyncagent.gateway.jpsSingletons import jpsBaseLibGW

# create a JVM module view and use it to import the required java classes
# TODO should this be part of AsyncAgent class?
# jpsBaseLib_view = jpsBaseLibGW.createModuleView()
# jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
# jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

def async_hello(whom):
    print('Async hello %s!' % whom)

class AsyncAgent(object):
    def __init__(self, app, task_id, agent_iri, time_interval, derivationClient, **configs):
        self.app = app
        self.configs(**configs)
        self.scheduler = APScheduler()
        self.interval_task_id = task_id
        self.time_interval = time_interval
        self.agentIRI = agent_iri
        # self.interval_task_id = 'interval-task-id'
        # self.time_interval = 5
        # self.agentIRI = 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service' # for testing purpose, will be provided as part of config
        # self.kgUrl = 'http://kg.cmclinnovations.com:81/blazegraph/namespaces/testontorxn/sparql' # for testing purpose
        # Initialise the derivationClient with SPARQL Query and Update endpoint
        # self.storeClient = jpsBaseLib_view.RemoteStoreClient(self.kgUrl, self.kgUrl)
        # self.derivationClient = jpsBaseLib_view.DerivationClient(self.storeClient)
        self.derivationClient = derivationClient
    
    def configs(self, **configs):
        for config, value in configs:
            self.app.config[config.upper()] = value
    
    def add_endpoint(self, endpoint=None, endpoint_name=None, handler=None, methods=['GET'], *args, **kwargs):
        self.app.add_url_rule(endpoint, endpoint_name, handler, methods=methods, *args, **kwargs)
    
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
        self.scheduler.add_job(id=self.interval_task_id, func=self.monitorDerivations, 
                               trigger='interval', seconds=self.time_interval)
        self.scheduler.start()
        self.app.run(**kwargs)




# flask_app = Flask(__name__)

# app = AsyncAgent(flask_app)

# if __name__ == "__main__":
#     app.run()
