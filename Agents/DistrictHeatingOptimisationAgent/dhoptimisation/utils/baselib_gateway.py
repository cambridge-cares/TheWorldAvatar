# The purpose of this module is to create and start JAVA resource gateway object to JPS_BASE_LIB

# As there seem to be issues with multiple JPS_BASE_LIB Java gateways, (i.e. initialising 
# a new jps gateway here while PySparqlClient relies on the gateway from pyderivationagent),
# the gateway created within pyderivationagent shall be used

from pyderivationagent.kg_operations.gateway import jpsBaseLibGW