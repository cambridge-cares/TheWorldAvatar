# The purpose of this module is to create and start a single
# JPSGateway object to be used in all of your other modules
#============================================================
from py4jps import JPSGateway

jps = JPSGateway()
jps.start()
#============================================================