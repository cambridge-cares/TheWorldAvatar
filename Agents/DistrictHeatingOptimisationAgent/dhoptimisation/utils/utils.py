# The purpose of this module is to provide some common utility methods

from py4jps import agentlogging

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def raise_error(errortype:Exception, msg):
    logger.error(msg)
    raise errortype(msg)
