# The purpose of this module is to create and start JAVA resource gateway object

from py4jps.resources import JpsBaseLib, StackClients

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

stackClientsGw = StackClients()
stackClientsGw.launchGateway()