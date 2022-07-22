# The purpose of this module is to create and start JAVA resource gateway object

from py4jps.resources import StackClients

stackclientsGw = StackClients()
stackclientsGw.launchGateway()

# Create module view to postgis client
# TODO: create view to all clients instead
stackclients_view = stackclientsGw.createModuleView()
stackclientsGw.importPackages(stackclients_view,"com.cmclinnovations.stack.clients.postgis.*")

# Testing that JAVA methods can successfully be called
stackclients_view.PostGISClient().toString()

