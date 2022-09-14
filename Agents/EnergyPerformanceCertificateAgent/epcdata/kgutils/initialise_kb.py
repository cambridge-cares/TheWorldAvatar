################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 13 Sep 2022                            #
################################################

# The purpose of this module is to initialise the knowledge 
# base with the OntoBuildingEnvironment TBox and Abox from TWA

import os
import requests

import agentlogging

from epcdata.datamodel.iris import *
from epcdata.kgutils.javagateway import jpsBaseLibGW
from epcdata.kgutils.kgclient import KGClient
from epcdata.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT


# Initialise logger
logger = agentlogging.get_logger("prod")


# URLs to .owl files
tbox = 'http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl'
abox = 'http://www.theworldavatar.com/kb/ontobuiltenv/OntoBuiltEnv.owl'

# Create KGclient to upload .owl files
kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
# Create a JVM module view to create Java File object
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

# Verify that TBox has not been initialized
try:
    query = f'SELECT * WHERE {{ <{OBE}> <{OWL_VERSION}> ?v}}'
    res = kg_client.performQuery(query)
except:
    logger.error("Unable to retrieve TBox version from KG.")
    raise Exception("Unable to retrieve TBox version from KG.")

if not res:
    # Upload TBox and ABox if not already instantiated
    temp_fp = 'tmp.owl'
    for i in [tbox, abox]:
        try:
            # Retrieve .owl file
            logger.info(f'Retrieving {i.capitalize()} from TWA ...')
            content = requests.get(i)
            logger.info(f'Writing temporary {i.capitalize()} .owl file ...')
            with open(temp_fp, 'w') as f:
                f.write(content.text)
            # Create Java file
            temp_f = jpsBaseLib_view.java.io.File(temp_fp)
            # Upload .owl file to KG
            logger.info(f'Uploading {i.capitalize()} .owl file to KG ...')
            kg_client.kg_client.uploadFile(temp_f)
            os.remove(temp_fp)
        except:
            logger.error("Unable to initialise knowledge base with TBox and ABox.")
            raise Exception("Unable to initialise knowledge base with TBox and ABox.")
