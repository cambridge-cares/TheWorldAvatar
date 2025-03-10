# The purpose of this module is to create and start JAVA resource gateway objects
# to the TWA/JPS base library to be used in other modules and scripts
# ===============================================================================
from twa import JPSGateway

jps_gateway = JPSGateway()
jps_gateway.launchGateway()