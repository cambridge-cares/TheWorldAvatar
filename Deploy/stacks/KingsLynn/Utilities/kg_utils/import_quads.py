################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# This module uploads all triples from an '.nq' file to
# an online Blazegraph SPARQL endpoint using JPS RemoteStoreClient

import os
from pathlib import Path
from py4jps.resources import JpsBaseLib

# Create and start JAVA resource gateway objects to JPS_BASE_LIB 
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# Create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")


def upload_all_quads(endpoint, filepath):

    # Initialise KG client
    kg_client = jpsBaseLib_view.RemoteStoreClient(endpoint, endpoint)
    
    # Create Java file object
    temp_f = jpsBaseLib_view.java.io.File(filepath)

    # Upload file to KG
    print('Uploading quads ...')
    kg_client.uploadFile(temp_f)
    print('Done.')



if __name__ == '__main__':

    # Specify SPARQL query endpoint
    # NOTE: Endpoint needs to be available, i.e. manually created beforehand
    endpoint = "http://localhost:9999/blazegraph/namespace/test/sparql"
    # Input file for triples (relative path)
    fp = r'..\data\outputs\ocgml.nq'

    # Get all Triples and serialise as turtle
    file_name = os.path.join(Path(__file__).parent, fp)
    upload_all_quads(endpoint, file_name)
