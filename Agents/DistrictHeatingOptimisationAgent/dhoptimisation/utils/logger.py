# The purpose of this module is to provide an aligned logger instance

from py4jps import agentlogging

from dhoptimisation.entry_point import logger_level

# Initialise logger instance (ensure consistent logger level)
logger = agentlogging.get_logger(logger_level)