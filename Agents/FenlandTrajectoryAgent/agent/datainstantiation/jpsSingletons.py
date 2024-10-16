# The purpose of this module is to create and start JAVA resource gateway objects
# to the TWA/JPS base library to be used in other modules and scripts
# ===============================================================================
from twa.resources import JpsBaseLib, StackClients

# # Instantiate and start resource gateway object to JPS_BASE_LIB
# jpsBaseLibGW = JpsBaseLib()
# jpsBaseLibGW.launchGateway()

stackClientsGw = StackClients()
stackClientsGw.launchGateway()

# Create a JVM module view and use it to import the required java classes
# jpsBaseLibView = jpsBaseLibGW.createModuleView()
# jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
# jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.timeseries.*")
# jpsBaseLibGW.importPackages(jpsBaseLibView, "org.postgis.Point")
