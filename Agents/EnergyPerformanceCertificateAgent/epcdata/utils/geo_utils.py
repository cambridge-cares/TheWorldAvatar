################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 26 Sep 2022                            #
################################################

# The purpose of this module is to provide several utility 
# functions to handle geospatial data (retrieved from OntoCityGml)

import pyproj

import agentlogging


# Initialise logger
logger = agentlogging.get_logger("prod")


def initialise_pyproj_transformer(current_crs: str, target_crs: str):
    """
        Returns pyproj transformer object for specified coordinate reference systems
    """

    # Initialise pyproj coordinate reference system (CRS) objects
    crs_in = pyproj.CRS.from_string(current_crs)
    crs_out = pyproj.CRS.from_string(target_crs)

    # Initialise pyproj CRS transformer
    tg = pyproj.transformer.TransformerGroup(crs_in, crs_out)
    # Ensure that most accurate transformation is available
    if not tg.best_available:
        tg.download_grids(verbose=True)
        # Update transformer to take effect after download
        tg = pyproj.transformer.TransformerGroup(crs_in, crs_out)
        if not tg.best_available:
            logger.warn('Best transformer for specified CRS transformation not available. Results may be inaccurate.')
    
    # Initialise actual transformer to use
    trans = pyproj.Transformer.from_crs(crs_in, crs_out, always_xy=True)

    return trans