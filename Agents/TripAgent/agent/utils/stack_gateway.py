# The purpose of this module is to create and start JAVA resource gateway object to STACK_CLIENTS

# The reason behind separating JPS_BASE_LIB and STACK_CLIENTS gateway objects is to
# avoid import issues when deploying the agent in "standalone" mode
# (i.e. ensure functionality without the StackClients resource installed via jpsrm)

from twa.resources import StackClients

stackClientsGw = StackClients()
stackClientsGw.launchGateway()

stack_clients_view = stackClientsGw.createModuleView()
stackClientsGw.importPackages(
    stack_clients_view, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient")
stackClientsGw.importPackages(
    stack_clients_view, "com.cmclinnovations.stack.clients.postgis.PostGISClient")
stackClientsGw.importPackages(
    stack_clients_view, "com.cmclinnovations.stack.clients.ontop.OntopClient")
