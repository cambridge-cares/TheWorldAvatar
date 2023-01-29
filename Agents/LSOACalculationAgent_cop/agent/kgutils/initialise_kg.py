################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 30 Oct 2022                            #
################################################
################################################
# Editor: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to initialise the knowledge graph,
# i.e. create Blazegraph namespace and upload ontology TBox and ABox

import os
import requests

import agentlogging

from agent.datamodel.iris import *
from agent.kgutils.kgclient import KGClient
from agent.errorhandling.exceptions import KGException
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT

# Initialise logger
logger = agentlogging.get_logger("prod")


def create_blazegraph_namespace(endpoint=UPDATE_ENDPOINT,
                                quads=False, geospatial=False):
    """
    Creates Blazegraph namespace with name as specified in SPARQL update endpoint

    Arguments:
        quads - Boolean Flag whether quad/triple namespace shall be created
        geospatial - Boolean flag whether to enable geospatial capabilities
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

'''
def upload_ontology(tbox_url=TBOX_URL_GASCOMPONENT, abox_url=ABOX_URL):
    """
    Uploads TBox and ABox to KG namespace

    NOTE: Uploading .owl files seems to create blank nodes when "collections" are
    used in the .owl file. This behaviour has been observed using both the fileUpload
    method of the RemoteStoreClient and using the owlready2 Python library.

    Arguments:
        tbox_url - URL to TBox
        abox_url - URL to ABox
    """

    # Create KGclient to upload .owl files
    kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)

    # Verify that TBox has not been initialized
    try:
        query = f'SELECT * WHERE {{ <http://www.theworldavatar.com/ontology/ontogasgrid/OntoGasGrid.owl> <{OWL_VERSION}> ?v}}'
        res = kg_client.performQuery(query)
    except Exception as ex:
        logger.error("Unable to retrieve TBox version from KG.")
        raise KGException("Unable to retrieve TBox version from KG.") from ex

    if not res:
        # Upload TBox and ABox if not already instantiated
        temp_fp = 'tmp.owl'
        for i in [tbox_url, abox_url]:
            try:
                # Retrieve .owl file
                logger.info(f'Retrieving {i} from TWA ...')
                try:
                    content = requests.get(i)
                    if content.status_code != 200:
                        raise Exception(f'HTTP error code for retrieving owl file: {content.status_code}')
                except Exception as ex:
                    logger.error(f"Unable to retrieve {i} from TWA server.")
                    raise KGException(f"Unable to retrieve {i} from TWA server.") from ex
                logger.info(f'Writing temporary {i} .owl file ...')
                with open(temp_fp, 'w') as f:
                    f.write(content.text)
                # Create Java file
                temp_f = kg_client.jpsBaseLib_view.java.io.File(temp_fp)
                # Upload .owl file to KG
                logger.info(f'Uploading {i} .owl file to KG ...')
                kg_client.kg_client.uploadFile(temp_f)
                os.remove(temp_fp)
            except Exception as ex:
                logger.error("Unable to initialise knowledge graph with TBox and ABox.")
                raise KGException("Unable to initialise knowledge graph with TBox and ABox.") from ex
'''