# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# For further information, please check the jps-base_lib, py4jps

from py4jps.resources import JpsBaseLib
# jpsBaseLibGW = JpsBaseLib()
# jpsBaseLibGW.launchGateway()

# ## create a JVM module view and use it to import the required java classes
# jpsBaseLib_view = jpsBaseLibGW.createModuleView()
# jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

def createMarkUpDerivation(belongsToEntitiesIRI:list, agentIRI:str, inputsIRI:list, derivationClient, forUpdate:bool = False) -> str:
    """
    Parameters
    ----------
    belongsToEntitiesIRI : list
        The list of IRI which links with the derivation via "belongsTo".
    agentIRI : str
        the agent IRI.
    inputsIRI : list
        The list of IRI which links with the derivation via "isDerivedFrom".
    storeClient : the store client instance of the java class RemoteStoreClient.
        storeClient identifies the kg endpoint information.
    forUpdate : bool, optional
        it is used to identify whether the agent is going to be used in the full derivation framework. The default is False for mark up use.

    Returns
    -------
    createdDerivation : String
        the derivation node IRI.

    """
    # jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")
    # ## set up the derivationInstanceBaseURL
    # derivationInstanceBaseURL = ukdt.baseURL + '/' + ukdt.topNode + '/'
    # ## initialise the derivationClient
    # derivationClient = jpsBaseLib_view.DerivationClient(storeClient, derivationInstanceBaseURL)
    # print(storeClient.getQueryEndpoint())
    ## create the createdDerivation
    createdDerivation = derivationClient.createAsyncDerivation(belongsToEntitiesIRI, agentIRI, inputsIRI, forUpdate)
    
    return createdDerivation

if __name__ == '__main__': 
    jpsBaseLibGW = JpsBaseLib()
    jpsBaseLibGW.launchGateway()

    ## create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
    endPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
    storeClient = jpsBaseLib_view.RemoteStoreClient(endPointURL, endPointURL)
    ModelInputVariableIRIList = ['http://www.theworldavatar.com/kb/ontopowsys/BusNumber_4a832bb7-a9f6-40c0-bb49-3a036a382dae', 'http://www.theworldavatar.com/kb/ontopowsys/BusType_bf8f7115-94d2-4b30-98d2-f0932df918f3', 'http://www.theworldavatar.com/kb/ontopowsys/PdBus_c4fa810b-131e-4805-b14a-10674e9f8769', 'http://www.theworldavatar.com/kb/ontopowsys/GdBus_138f59ae-ca18-4f47-bbb6-edf260e8e77c', 'http://www.theworldavatar.com/kb/ontopowsys/Gs_06a10ba8-397a-4693-a981-30a0575a68f8', 'http://www.theworldavatar.com/kb/ontopowsys/Bs_dadf103f-d40a-4782-b8f1-fcc24d0e1a76', 'http://www.theworldavatar.com/kb/ontopowsys/Area_c3c0cde4-9718-4147-888f-b001861d7557', 'http://www.theworldavatar.com/kb/ontopowsys/Vm_351d012b-14f3-4f3b-834b-c0a358863507', 'http://www.theworldavatar.com/kb/ontopowsys/Va_cfe7d283-da92-4dc7-8017-70979b4f1e1c', 'http://www.theworldavatar.com/kb/ontopowsys/baseKV_fc33f334-aa88-4927-9b5f-2b1a30ddd502', 'http://www.theworldavatar.com/kb/ontopowsys/Zone_3720a687-5e53-4ffd-8cfc-1dd2c39c743e', 'http://www.theworldavatar.com/kb/ontopowsys/VmMax_725c736d-eb00-4c0a-aa9f-84f84a631e33', 'http://www.theworldavatar.com/kb/ontopowsys/VmMin_34de8974-29ea-4a8c-99a3-f38a99b0a921']
    BusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_9fee5b07-8a08-4a60-93e2-bc118bce3965"

    storeClient.executeUpdate("delete where {?s ?p ?o}")
    
    createdDerivation = createMarkUpDerivation(ModelInputVariableIRIList, AgentIRI, [BusNodeIRI], storeClient, False)
    print(createdDerivation)