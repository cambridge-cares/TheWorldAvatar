# The purpose of this module is to create and start JAVA resource gateway object

from py4jps.resources import JpsBaseLib

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
