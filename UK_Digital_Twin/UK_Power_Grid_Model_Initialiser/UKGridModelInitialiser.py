##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 April 2022        #
##########################################

import os, sys
BASE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, BASE)
from jpsSingletons import jpsBaseLibGW
from datetime import datetime
import pytz
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
import uuid

class gridModelInitialiser(object):
    
    
    def __init__(self, topologyNodeIRI:str, startTime_of_EnergyConsumption:str, agentIRI:str, slackBusNodeIRI:str, endPointURL:str, endPointUser:str = None, endPointPassWord:str = None,\
                 powerPlantOWLFileLocalPath = None, updateLocalPowerPlantOWLFileFlag:bool = True):
        
        ## specify the topology node
        self.topologyNodeIRI = topologyNodeIRI
        
        ## create the power system model node IRI
        self.powerSystemModelIRI = UK_PG.ontopowsys_namespace + UK_PG.powerSystemModelKey + str(uuid.uuid4())
        
        ## create the timeStamp
        self.timeStamp = datetime.now(pytz.utc).isoformat()
        
        ## specify the startTime_of_EnergyConsumption for querying the demand load 
        self.startTime_of_EnergyConsumption = startTime_of_EnergyConsumption
        
        ## specify the agent IRI        
        self.agentIRI = agentIRI
        
        ## specify the slackBusNodeIRI (there is only one slack bus is allowed in the modelling)
        self.slackBusNodeIRI = slackBusNodeIRI
        
        ## specify the query/update endpoint information
        self.endPointURL = endPointURL
        self.endPointUser = endPointUser
        self.endPointPassWord = endPointPassWord
        
        ## specify the local storage path (to be deleted)
        self.powerPlantOWLFileLocalPath = powerPlantOWLFileLocalPath
        self.updateLocalPowerPlantOWLFileFlag = updateLocalPowerPlantOWLFileFlag       
        
        
        ## create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
        # jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")
        
        ## initialise the derivationClient with SPARQL Query and Update endpoint
        if self.endPointUser is None:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.endPointURL, self.endPointURL)
        else:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.endPointURL, self.endPointURL, self.endPointUser, self.endPointPassWord)
        
        
        
        
    def ModelInputInitialiser(self):
        
        return
    
if __name__ == '__main__':    
    test = gridModelInitialiser("", "")
    
    print(test.powerSystemModel)
    print(test.timeStamp)