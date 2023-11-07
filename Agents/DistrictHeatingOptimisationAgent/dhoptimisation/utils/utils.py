# The purpose of this module is to provide some common utility methods

from .logger import logger

def raise_error(errortype:Exception, msg):
    logger.error(msg)
    raise errortype(msg)
