# The purpose of this module is to provide some common utility methods

from py4jps import agentlogging

from dhoptimisation.entry_point import logger_level

# Initialise aligned logger instance (ensure consistent logger level throughout!)
logger = agentlogging.get_logger(logger_level)


def raise_error(errortype:Exception, msg):
    logger.error(msg)
    raise errortype(msg)
