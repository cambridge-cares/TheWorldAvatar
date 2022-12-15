# The purpose of this module is to create and start JAVA resource gateway objects
# to JPS_BASE_LIB and STACK_CLIENTS

from py4jps.resources import JpsBaseLib

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

