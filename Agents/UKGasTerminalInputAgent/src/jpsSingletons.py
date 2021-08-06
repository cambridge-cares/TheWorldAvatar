# The purpose of this module is to create and start resource
# gateway objects to be used in all your other modules
#============================================================
from py4jps.resources import JpsBaseLib

# Instantiate and start resource gateway object to JPS_BASE_LIB
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
