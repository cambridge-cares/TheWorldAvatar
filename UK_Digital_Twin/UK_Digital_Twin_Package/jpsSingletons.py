# The purpose of this module is to create and start resource
# gateway objects to be used in all your other modules
#============================================================
from py4jps.resources import JpsBaseLib

# jpsBaseLib resource gateway
# you may also wish to pass any **JGkwargs
jpsBaseLibGW = JpsBaseLib()

# you may also wish to pass any **LGkwargs
jpsBaseLibGW.launchGateway()