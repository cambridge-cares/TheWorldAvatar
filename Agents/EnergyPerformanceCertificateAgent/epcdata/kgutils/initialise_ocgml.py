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
    except Exception as ex:
        logger.error("Unable to upload OntoCityGml quads file.")
        raise KGException("Unable to upload OntoCityGml quads file.") from ex
