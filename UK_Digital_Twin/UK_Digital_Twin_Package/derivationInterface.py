# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# For further information, please check the jps-base_lib, py4jps

###############################################
# Extended by: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 April 2022             #
###############################################

import os, sys
BASE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, BASE)
from jpsSingletons import jpsBaseLibGW
import uuid

## create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")


def createMarkUpDerivation(belongsToEntitiesIRI:list, agentIRI:str, inputsIRI:list, storeClient, forUpdate:bool = False) -> str:
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
    ## set up the derivationInstanceBaseURL
    derivationInstanceBaseURL = jpsBaseLib_view.DerivationSparql.derivednamespace
    ## initialise the derivationClient
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient, derivationInstanceBaseURL)
    ## create the createdDerivation
    createdDerivation = derivationClient.createAsyncDerivation(belongsToEntitiesIRI, agentIRI, inputsIRI, forUpdate)
    
    return createdDerivation


# if __name__ == '__main__':  
    