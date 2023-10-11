################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the actual optimisation logic and
# methods for the DHOptimisationAgent

import pandas as pd
import CoolProp.CoolProp as CP

from py4jps import agentlogging

from dhoptimisation.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def raise_error(errortype:Exception, msg):
    logger.error(msg)
    raise errortype(msg)