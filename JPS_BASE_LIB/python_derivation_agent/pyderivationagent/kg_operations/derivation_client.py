from pyderivationagent.kg_operations.gateway import jpsBaseLibGW
from pyderivationagent.data_model.derivation import Derivation

from typing import List, Dict


class PyDerivationClient:
    """This is a wrapper class for the java class uk.ac.cam.cares.jps.base.derivation.DerivationClient.
    Only the methods that commonly used are wrapped here. For other methods, one may access them via
    self.derivation_client.javaMethod(args).
    """

    def __init__(
        self,
        derivation_instance_base_url: str,
        query_endpoint: str,
        update_endpoint: str,
        kg_user:str=None,
        kg_password:str=None
    ) -> None:
        """Initialize the derivation client

        Args:
            derivation_instance_base_url (str): base url of the derivation instance to be created by the client
            query_endpoint (str): query endpoint of the knowledge graph
            update_endpoint (str): update endpoint of the knowledge graph
            kg_user (str, optional): username for the knowledge graph endpoint. Defaults to None.
            kg_password (str, optional): password for the knowledge graph endpoint. Defaults to None.
        """

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
        """Create a normal derivation markup.

        Args:
            entities (List[str]): List of entities that belongsTo the derivation
            agentIRI (str): List of agents that the derivation isDerivedUsing
            inputs (List[str]): List of inputs that the derivation isDerivedFrom

        Returns:
            str: IRI of the created derivation
        """
        return self.derivation_client.createDerivation(entities, agentIRI, inputs)


    def createSyncDerivationForNewInfo(
        self,
        agentIRI: str,
        inputsIRI: List[str],
        derivationType: str
    ) -> Derivation:
        """Create a sync derivation for new info to be computed by the agent.

        Args:
            agentIRI (str): IRI of the agent that the derivation isDerivedUsing
            inputsIRI (List[str]): List of inputs that the derivation isDerivedFrom
            derivationType (str): IRI of the synchronous derivation type to be created

        Returns:
            Derivation: Object of the created derivation
        """
        derivation_java = self.derivation_client.createSyncDerivationForNewInfo(agentIRI, inputsIRI, derivationType)
        return Derivation(derivation_java=derivation_java)


    def createSyncDerivationForNewInfoWithHttpUrl(
        self,
        agentIRI: str,
        agentURL: str,
        inputsIRI: List[str],
        derivationType: str
    ) -> Derivation:
        """Create a sync derivation for new info to be computed by the agent with the agent url provided.

        Args:
            agentIRI (str): IRI of the agent that the derivation isDerivedUsing
            agentURL (str): HTTP URL of the agent that the derivation isDerivedUsing
            inputsIRI (List[str]): List of inputs that the derivation isDerivedFrom
            derivationType (str): IRI of the synchronous derivation type to be created

        Returns:
            Derivation: Object of the created derivation
        """
        derivation_java = self.derivation_client.createSyncDerivationForNewInfo(agentIRI, agentURL, inputsIRI, derivationType)
        return Derivation(derivation_java=derivation_java)


    def bulkCreateDerivations(
        self,
        entitiesList: List[List[str]],
        agentIRIList: List[str],
        inputsList: List[List[str]]
    ) -> List[str]:
        """Create multiple normal derivations in one go.

        Args:
            entitiesList (List[List[str]]): List of list of entities that belongsTo the derivations
            agentIRIList (List[str]): List of agents that the derivations isDerivedUsing
            inputsList (List[List[str]]): List of list of inputs that the derivations isDerivedFrom

        Returns:
            List[str]: List of IRIs of the created derivations
        """
        return self.derivation_client.bulkCreateDerivations(entitiesList, agentIRIList, inputsList)


    def createDerivationWithTimeSeries(
        self,
        entities: List[str],
        agentIRI: str,
        inputs: List[str]
    ) -> str:
        """Create a time series derivation markup.

        Args:
            entities (List[str]): List of entities that belongsTo the derivation
            agentIRI (str): IRI of the agent that the derivation isDerivedUsing
            inputs (List[str]): List of inputs that the derivation isDerivedFrom

        Returns:
            str: IRI of the created time series derivation
        """
        return self.derivation_client.createDerivationWithTimeSeries(entities, agentIRI, inputs)


    def bulkCreateDerivationsWithTimeSeries(
        self,
        entitiesList: List[List[str]],
        agentIRIList: List[str],
        inputsList: List[List[str]]
    ) -> List[str]:
        """Create multiple time series derivations in one go.

        Args:
            entitiesList (List[List[str]]): List of list of entities that belongsTo the derivations
            agentIRIList (List[str]): List of agents that the derivations isDerivedUsing
            inputsList (List[List[str]]): List of list of inputs that the derivations isDerivedFrom

        Returns:
            List[str]: List of IRIs of the created time series derivations
        """
        return self.derivation_client.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIRIList, inputsList)


    def createAsyncDerivation(
        self,
        entities: List[str],
        agentIRI: str,
        inputs: List[str],
        forUpdate: bool
    ) -> str:
        """Create an asynchronous derivation markup. If `forUpdate` is True, the derivation will be marked as "Requested"
        with a timestamp of 0. Otherwise, the derivation will be marked without status but with a current timestamp.

        Args:
            entities (List[str]): List of entities that belongsTo the derivation
            agentIRI (str): IRI of the agent that the derivation isDerivedUsing
            inputs (List[str]): List of inputs that the derivation isDerivedFrom
            forUpdate (bool): Boolean flag to indicate if the derivation markup is for update

        Returns:
            str: IRI of the created asynchronous derivation
        """
        return self.derivation_client.createAsyncDerivation(entities, agentIRI, inputs, forUpdate)


    def createAsyncDerivationFromDerivation(
        self,
        entities: List[str],
        agentIRI: str,
        derivation: str,
        forUpdate: bool
    ) -> str:
        """Create an asynchronous derivation markup from an existing derivation. If `forUpdate` is True, the derivation
        will be marked as "Requested" with a timestamp of 0. Otherwise, the derivation will be marked without status but
        with a current timestamp.

        Args:
            entities (List[str]): List of entities that belongsTo the derivation
            agentIRI (str): IRI of the agent that the derivation isDerivedUsing
            derivation (str): IRI of an existing derivation whose outputs the new derivation isDerivedFrom
            forUpdate (bool): Boolean flag to indicate if the derivation markup is for update

        Returns:
            str: IRI of the created asynchronous derivation
        """
        return self.derivation_client.createAsyncDerivation(entities, agentIRI, derivation, forUpdate)


    def bulkCreateAsyncDerivations(
        self,
        entitiesList: List[List[str]],
        agentIRIList: List[str],
        inputsList: List[List[str]],
        forUpdateFlagList: List[bool]
    ) -> List[str]:
        """Create multiple asynchronous derivations in one go. If the flag in `forUpdateFlagList` is True, the corresponding
        derivation will be marked as "Requested" with a timestamp of 0. Otherwise, the derivation will be marked without
        status but with a current timestamp.

        Args:
            entitiesList (List[List[str]]): List of list of entities that belongsTo the derivations
            agentIRIList (List[str]): List of agents that the derivations isDerivedUsing
            inputsList (List[List[str]]): List of list of inputs that the derivations isDerivedFrom
            forUpdateFlagList (List[bool]): List of boolean flags to indicate if the derivation markup is for update

        Returns:
            List[str]: List of IRIs of the created asynchronous derivations
        """
        return self.derivation_client.bulkCreateAsyncDerivations(entitiesList, agentIRIList, inputsList, forUpdateFlagList)


    def createAsyncDerivationForNewInfo(
        self,
        agentIRI: str,
        inputsAndDerivations: List[str]
    ) -> str:
        """Create an asynchronous derivation markup for new information. The derivation will be marked as "Requested" with
        a timestamp of 0. The outputs of the derivation will be computed by the agent in due course. Note that all IRIs in
        the `inputsAndDerivations` list will be directly connected to the derivation as its inputs. Therefore, existing IRIs
        of the derivation outputs should be provided if applicable, instead of the IRI of that derivation.

        Args:
            agentIRI (str): IRI of the agent that the derivation isDerivedUsing
            inputsAndDerivations (List[str]): List of inputs and derivations that the derivation isDerivedFrom

        Returns:
            str: IRI of the created asynchronous derivation for new information
        """
        return self.derivation_client.createAsyncDerivationForNewInfo(agentIRI, inputsAndDerivations)


    def bulkCreateAsyncDerivationsForNewInfo(
        self,
        agentIRIList: List[str],
        inputsAndDerivationsList: List[List[str]]
    ) -> List[str]:
        """Create multiple asynchronous derivations for new information in one go. The derivations will be marked as
        "Requested" with a timestamp of 0. The outputs of the derivations will be computed by the agents in due course.
        Note that all IRIs in the `inputsAndDerivationsList` list will be directly connected to the derivations as their
        inputs. Therefore, existing IRIs of the derivation outputs should be provided if applicable, instead of the IRI
        of that derivation.

        Args:
            agentIRIList (List[str]): List of agents that the derivations isDerivedUsing
            inputsAndDerivationsList (List[List[str]]): List of list of inputs and derivations that the derivations isDerivedFrom

        Returns:
            List[str]: List of IRIs of the created asynchronous derivations for new information
        """
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
        """Register an OntoAgent instance in the triple store.

        Args:
            ontoAgentServiceIRI (str): IRI of the agent's OntoAgent:Service
            ontoAgentOperationHttpUrl (str): IRI of the agent's operation HTTP URL
            inputTypes (List[str]): List of IRI of the agent's input types
            outputTypes (List[str]): List of IRI of the agent's output types
        """
        self.derivation_client.createOntoAgentInstance(
            ontoAgentServiceIRI, ontoAgentOperationHttpUrl, inputTypes, outputTypes)


    #####################################
    ## Methods for handling timestamps ##
    #####################################
    def addTimeInstance(self, entities: List[str]):
        """Add a time instance to each entity in the list of entities.

        Args:
            entities (List[str]): List of IRIs of entities to add time instance to
        """
        self.derivation_client.addTimeInstance(entities)


    def addTimeInstanceCurrentTimestamp(self, entities: List[str]):
        """Add time instance with current timestamp to the given entities

        Args:
            entities (List[str]): List of IRIs of entities to add time instance to
        """
        self.derivation_client.addTimeInstanceCurrentTimestamp(entities)


    def updateTimestamp(self, entity: str):
        """Update the timestamp of the entity to the current time.

        Args:
            entity (str): IRI of the entity to update the timestamp of
        """
        self.derivation_client.updateTimestamp(entity)


    def updateTimestamps(self, entities: List[str]):
        """Update the timestamp of all entities in the list.

        Args:
            entities (List[str]): List of IRIs of entities to update the timestamp for
        """
        self.derivation_client.updateTimestamps(entities)


    ##############################################
    ## Methods for requesting update derivation ##
    ##############################################
    def getDerivationsOf(self, entities: List[str]) -> Dict[str, str]:
        """Get the derivations of the given entities.

        Args:
            entities (List[str]): List of entities

        Returns:
            Dict[str, str]: The dictionary with the entity IRIs as keys and the derivation IRIs as values
        """
        return self.derivation_client.getDerivationsOf(entities)


    def unifiedUpdateDerivation(self, derivationIRI: str):
        """Unified update derivation method.

        Args:
            derivationIRI (str): IRI of the derivation to be updated
        """
        self.derivation_client.unifiedUpdateDerivation(derivationIRI)
