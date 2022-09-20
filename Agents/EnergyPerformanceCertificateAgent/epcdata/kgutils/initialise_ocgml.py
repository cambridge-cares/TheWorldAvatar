################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 20 Sep 2022                            #
################################################

# The purpose of this module is to initialise the OntoCityGml
# SPARQL endpoint and upload a provided RDF file (with quads)

import os
from pathlib import Path
import requests

import agentlogging

from epcdata.datamodel.iris import *
from epcdata.errorhandling.exceptions import KGException
from epcdata.kgutils.javagateway import jpsBaseLibGW
from epcdata.kgutils.kgclient import KGClient
from epcdata.utils.env_configs import OCGML_ENDPOINT


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


def upload_ocgml_quads(endpoint=OCGML_ENDPOINT):
    """
        Uploads (previously exported) OntoCityGml quads into specified KG endpoint
    """

    # File path where to expect .nq file to upload
    root = Path(__file__).parent
    rel_file_path = '../../data/data.nq'
    file_path = str(Path.joinpath(root, rel_file_path))

    # Verify that file to upload exists
    if not os.path.exists(file_path):
        logger.error('Provided file path to .nq file does not exist.')
        raise ValueError('Provided file path to .nq file does not exist.')

    # Create KG client to upload .nq file
    kg_client = KGClient(endpoint, endpoint)
    # Create a JVM module view to create Java File object
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

    try:
        # Create Java file
        temp_f = jpsBaseLib_view.java.io.File(file_path)
        # Upload .nq file to KG
        logger.info('Uploading OntoCityGml quads to KG ...')
        kg_client.kg_client.uploadFile(temp_f)
    except:
        logger.error("Unable to upload OntoCityGml quads file.")
        raise KGException("Unable to upload OntoCityGml quads file.")
