##############################################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk), John Atherton (ja685@cam.ac.uk)   #
# Date: 30 June 2023                                                         #
##############################################################################

# The purpose of this module is to create and start JAVA resource gateway objects
# to be used in other modules and scripts

from py4jps.resources import JpsBaseLib, StackClients

# Instantiates and starts a resource gateway object ot provide access to JPS_BASE_LIB
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# Instantiates and starts a resource gateway object to provide access to stack-clients
stackClientsGw = StackClients()
stackClientsGw.launchGateway()