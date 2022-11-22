# The purpose of this module is to create and start JAVA resource gateway objects
# to JPS_BASE_LIB and STACK_CLIENTS

# Please note: There seem to be issues with multiple JPS_BASE_LIB Java gateways,
#              i.e. initialising a new jps gateway here while PySparqlClient relies
#              on the gateway from pyderivationagent
# Hence, the gateway created within pyderivationagent shall be used

from pyderivationagent.kg_operations.gateway import jpsBaseLibGW

from py4jps.resources import StackClients

stackClientsGw = StackClients()
stackClientsGw.launchGateway()
