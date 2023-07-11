# The purpose of this module is to create and start JAVA resource gateway object to STACK_CLIENTS

# The reason behind separating JPS_BASE_LIB and STACK_CLIENTS gateway objects is to 
# avoid import issues when deploying the agent in "standalone" mode 
# (i.e. ensure functionality without the StackClients resource installed via jpsrm)

from py4jps.resources import StackClients

stackClientsGw = StackClients()
stackClientsGw.launchGateway()