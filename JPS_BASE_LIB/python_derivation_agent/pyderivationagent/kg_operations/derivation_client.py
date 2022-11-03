from pyderivationagent.kg_operations.gateway import jpsBaseLibGW
from pyderivationagent.data_model.derivation import Derivation

from typing import List, Dict


class PyDerivationClient:
    def __init__(
        self,
        derivation_instance_base_url,
        query_endpoint,
        update_endpoint,
        kg_user=None,
        kg_password=None
    ) -> None:
        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

        # create store_client
        if kg_user is None:
            store_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)
        else:
            store_client = self.jpsBaseLib_view.RemoteStoreClient(
                query_endpoint, update_endpoint, kg_user, kg_password)

        # create derivation_client
        self.derivation_client = self.jpsBaseLib_view.DerivationClient(store_client, derivation_instance_base_url)


    ############################################
    ## Methods for creating derivation markup ##
    ############################################
    def createDerivation(
        self,
        entities: List[str],
        agentIRI: str,
        inputs: List[str]
    ) -> str:
        return self.derivation_client.createDerivation(entities, agentIRI, inputs)


    def createSyncDerivationForNewInfo(
        self,
        agentIRI: str,
        inputsIRI: List[str],
        derivationType: str
    ) -> Derivation:
        derivation_java = self.derivation_client.createSyncDerivationForNewInfo(agentIRI, inputsIRI, derivationType)
        return Derivation(derivation_java=derivation_java)


    def createSyncDerivationForNewInfoWithHttpUrl(
        self,
        agentIRI: str,
        agentURL: str,
        inputsIRI: List[str],
        derivationType: str
    ) -> Derivation:
        derivation_java = self.derivation_client.createSyncDerivationForNewInfo(agentIRI, agentURL, inputsIRI, derivationType)
        return Derivation(derivation_java=derivation_java)


    def bulkCreateDerivations(
        self,
        entitiesList: List[List[str]],
        agentIRIList: List[str],
        inputsList: List[List[str]]
    ) -> List[str]:
        return self.derivation_client.bulkCreateDerivations(entitiesList, agentIRIList, inputsList)


    def createDerivationWithTimeSeries(
        self,
        entities: List[str],
        agentIRI: str,
        inputs: List[str]
    ) -> str:
        return self.derivation_client.createDerivationWithTimeSeries(entities, agentIRI, inputs)


    def bulkCreateDerivationsWithTimeSeries(
        self,
        entitiesList: List[List[str]],
        agentIRIList: List[str],
        inputsList: List[List[str]]
    ) -> List[str]:
        return self.derivation_client.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIRIList, inputsList)


    def createAsyncDerivation(
        self,
        entities: List[str],
        agentIRI: str,
        inputs: List[str],
        forUpdate: bool
    ) -> str:
        return self.derivation_client.createAsyncDerivation(entities, agentIRI, inputs, forUpdate)


    def createAsyncDerivationFromDerivation(
        self,
        entities: List[str],
        agentIRI: str,
        derivation: str,
        forUpdate: bool
    ) -> str:
        return self.derivation_client.createAsyncDerivation(entities, agentIRI, derivation, forUpdate)


    def bulkCreateAsyncDerivations(
        self,
        entitiesList: List[List[str]],
        agentIRIList: List[str],
        inputsList: List[List[str]],
        forUpdateFlagList: bool
    ) -> List[str]:
        return self.derivation_client.bulkCreateAsyncDerivations(entitiesList, agentIRIList, inputsList, forUpdateFlagList)


    def createAsyncDerivationForNewInfo(
        self,
        agentIRI: str,
        inputsAndDerivations: List[str]
    ) -> str:
        return self.derivation_client.createAsyncDerivationForNewInfo(agentIRI, inputsAndDerivations)


    def bulkCreateAsyncDerivationsForNewInfo(
        self,
        agentIRIList: List[str],
        inputsAndDerivationsList: List[List[str]]
    ) -> List[str]:
        return self.derivation_client.bulkCreateAsyncDerivationsForNewInfo(agentIRIList, inputsAndDerivationsList)


    #############################################
    ## Methods for creating OntoAgent instance ##
    #############################################
    def createOntoAgentInstance(
        self,
        ontoAgentServiceIRI: str,
        ontoAgentOperationHttpUrl: str,
        inputTypes: List[str],
        outputTypes: List[str]
    ):
        self.derivation_client.createOntoAgentInstance(
            ontoAgentServiceIRI, ontoAgentOperationHttpUrl, inputTypes, outputTypes)


    #####################################
    ## Methods for handling timestamps ##
    #####################################
    def addTimeInstance(self, entities: List[str]):
        self.derivation_client.addTimeInstance(entities)


    def addTimeInstanceCurrentTimestamp(self, entities: List[str]):
        self.derivation_client.addTimeInstanceCurrentTimestamp(entities)


    def updateTimestamp(self, entity: List[str]):
        self.derivation_client.updateTimestamp(entity)


    def updateTimestamps(self, entities: List[str]):
        self.derivation_client.updateTimestamps(entities)


    ##############################################
    ## Methods for requesting update derivation ##
    ##############################################
    def getDerivationsOf(self, entities) -> Dict[str, str]:
        return self.derivation_client.getDerivationsOf(entities)


    def unifiedUpdateDerivation(self, derivationIRI):
        self.derivation_client.unifiedUpdateDerivation(derivationIRI)
