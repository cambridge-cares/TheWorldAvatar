##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2021         #
##########################################

"""This module is used to recored the repository labels used in the remote triple store, Blazegraph. The lables will be refered when performing queries."""

"""The lables of different repositories maintained in RDF4j triple store deployed in CoMo server"""

UKDigitalTwinKG = { # the repo used to store the top node graph (lookup table)
    'lable': 'ukdigitaltwin',
    'endpoint_iri' : "http://www.theworldavatar.com/kb/ontokgrouter/ukdigitaltwin",
    'queryendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin",
    'updateendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin/statements"}

UKPowerPlantKG = { # level 1
    'lable': 'ukpowerplant',
    'endpoint_iri' : "http://www.theworldavatar.com/kb/ontokgrouter/ukpowerplant",
    'queryendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlant",
    'updateendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlant/statements"}

UKEnergyConsumptionKG = { # level 1
    'lable': 'ukenergyconsumptionkg',
    'endpoint_iri' : "http://www.theworldavatar.com/kb/ontokgrouter/ukenergyconsumptionkg",
    'queryendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG",
    'updateendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG/statements"}

UKGridTopologylKG = { # level 2
    'lable': 'ukpowergridtopology',
    'endpoint_iri' : "http://www.theworldavatar.com/kb/ontokgrouter/ukpowergridtopology",
    'queryendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology",
    'updateendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology/statements"}

UKPowerGridModelKG = { # level 3
    'lable': 'ukpowergridmodel',
    'endpoint_iri' : "http://www.theworldavatar.com/kb/ontokgrouter/ukpowergridmodel",
    'queryendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridModel",
    'updateendpoint_iri' : "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridModel/statements"}

"""The label used to denote the lookup table (a navigating map ) in Blazegraph as a query router and its endpoint URL"""

lookupTable = {
    'lable': 'ontokgrouter',
    'endpoint_iri' : "http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql"}