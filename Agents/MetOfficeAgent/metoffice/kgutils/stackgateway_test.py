# The purpose of this module is to create and start JAVA resource gateway object

from py4jps.resources import StackClients

stackClientsGw = StackClients()
stackClientsGw.launchGateway()

# Create module view to postgis client
# TODO: create view to all clients instead
stackClientsView = stackClientsGw.createModuleView()
stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.docker.ContainerClient")
stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")

# Testing that JAVA methods can successfully be called
containerClient = stackClientsView.ContainerClient()
bg = stackClientsView.BlazegraphEndpointConfig("","","","","")
bg.getClass()
containerClient.readEndpointConfig("blazegraph", bg.getClass())
