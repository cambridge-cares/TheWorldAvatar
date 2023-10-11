# The purpose of this module is to create and start JAVA resource gateway objects
# to the TWA/JPS base library to be used in other modules and scripts
# ===============================================================================
from py4jps.resources import JpsBaseLib, StackClients

# Instantiate and start resource gateway object to JPS_BASE_LIB
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

stackClientsGw = StackClients()
stackClientsGw.launchGateway()

