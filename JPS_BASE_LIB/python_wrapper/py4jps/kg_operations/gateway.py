# The purpose of this module is to create and start JAVA resource gateway objects
# to be used in other modules and scripts
# ===============================================================================
from py4jps.resources import JpsBaseLib

# To avoid unnecessary logging information from py4j package, set logger level before
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

# Instantiate and start resource gateway object to JPS_BASE_LIB
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
