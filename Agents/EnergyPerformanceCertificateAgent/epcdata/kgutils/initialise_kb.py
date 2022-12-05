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
from epcdata.datamodel.data_mapping import GBP, GBP_PER_SM
from epcdata.errorhandling.exceptions import KGException
from epcdata.kgutils.javagateway import jpsBaseLibGW
from epcdata.kgutils.kgclient import KGClient
from epcdata.utils.env_configs import OCGML_ENDPOINT
from epcdata.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT


# Initialise logger
logger = agentlogging.get_logger("prod")


def create_blazegraph_namespace(endpoint=OCGML_ENDPOINT,
                                quads=True, geospatial=True):
    """
        Creates Blazegraph namespace with name as specified in SPARQL update endpoint
    """

    # Turn boolean flags for quads and geospatial into strings
    quads = str(quads).lower()
    geospatial = str(geospatial).lower()

    # Extract Blazegraph REST API url from SPARQL endpoint
    url = endpoint[:endpoint.find('namespace') + len('namespace')]

    # Extract name for new namespace from SPARQL endpoint
    ns = endpoint[endpoint.find('namespace') + len('namespace') + 1:]
    ns = ns[:ns.find('/')]

    # Define POST request header and payload
    header = {'Content-type': 'text/plain'}

    payload = 'com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n' \
              'com.bigdata.rdf.sail.isolatableIndices=false\r\n' \
              'com.bigdata.rdf.sail.truthMaintenance=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n' \
              'com.bigdata.namespace.{}.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n' \
              'com.bigdata.rdf.sail.namespace={}\r\n' \
              f'com.bigdata.rdf.store.AbstractTripleStore.quads={quads}\r\n' \
              'com.bigdata.namespace.{}.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n' \
              f'com.bigdata.rdf.store.AbstractTripleStore.geoSpatial={geospatial}\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false'.format(ns, ns, ns)

    # Post the request
    response = requests.post(url, payload, headers=header)

    if response.status_code == 201:
        logger.info('New namespace \"{}\" successfully created.\n'.format(ns))
    elif response.status_code == 409:
        logger.info('Namespace \"{}\" already exists\n'.format(ns))
    else:
        logger.info('Request status code: {}\n'.format(response.status_code))


def instantiate_GBP_units():
    """
        Return SPARQL update to instantiate required GBP units
    """

    # NOTE: There are reported issues with encoding of special characters, i.e. Blazegraph
    #       claiming to use utf-8 encoding while actually using iso-8859-1
    #       --> PoundSterling displayed wrongly in GUI but corrected when retrieved in code

    query = f"""
        INSERT DATA {{
            <{UOM_GBP_M2}> <{OM_SYMBOL}> \"{GBP_PER_SM}\"^^<{XSD_STRING}> . 
            <{OM_GBP}> <{OM_SYMBOL}> \"{GBP}\"^^<{XSD_STRING}> . 
    }}"""

    return query

def initialise_kb():
    """
        Uploads TBox and ABox from TWA to KG namespace
    """

    # URLs to .owl files
    #TODO: Potentially to be replaced with better maintained Github Links
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
    except Exception as ex:
        logger.error("Unable to retrieve TBox version from KG.")
        raise KGException("Unable to retrieve TBox version from KG.") from ex

    if not res:
        # Upload TBox and ABox if not already instantiated
        temp_fp = 'tmp.owl'
        for i in [tbox, abox]:
            try:
                # Retrieve .owl file
                logger.info(f'Retrieving {i.capitalize()} from TWA ...')
                try:
                    content = requests.get(i)
                except Exception as ex:
                    logger.error(f"Unable to retrieve {i.capitalize()} from TWA server.")
                    raise KGException(f"Unable to retrieve {i.capitalize()} from TWA server.") from ex
                logger.info(f'Writing temporary {i.capitalize()} .owl file ...')
                with open(temp_fp, 'w') as f:
                    f.write(content.text)
                # Create Java file
                temp_f = jpsBaseLib_view.java.io.File(temp_fp)
                # Upload .owl file to KG
                logger.info(f'Uploading {i.capitalize()} .owl file to KG ...')
                kg_client.kg_client.uploadFile(temp_f)
                os.remove(temp_fp)
            except Exception as ex:
                logger.error("Unable to initialise knowledge base with TBox and ABox.")
                raise KGException("Unable to initialise knowledge base with TBox and ABox.") from ex
        
        # Upload GBP symbols to KG (not directly part of OntoBuiltEnv ontology TBox or ABox,
        # but required within KG for proper agent execution later on (i.e. derivation agents,
        # and visualisation purposes)
        logger.info('Instantiating GBP symbols ...')
        query = instantiate_GBP_units()
        try:
            kg_client.performUpdate(query)
        except Exception as ex:
            logger.error("Unable to initialise GBP symbols in KG.")
            raise KGException("Unable to initialise GBP symbols in KG.") from ex

