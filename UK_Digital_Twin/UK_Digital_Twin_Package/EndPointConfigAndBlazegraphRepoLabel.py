"""This module is used to recored the repository labels used in the remote triple store, Blazegraph. The lables will be refered when performing queries."""

"""The lables of different repositories maintained in RDF4j triple store deployed in CoMo server"""

import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.endPoint import ONSEndpoint, UKPowerSystemBaseWorld, UKPopulationData

"""UK Power System Base World in Blazegraph"""
UKPowerSystemBaseWorld = {
    'label': 'UKPowerSystemBaseWorld',
    'endpoint_iri' : UKPowerSystemBaseWorld,
    'queryendpoint_iri' : UKPowerSystemBaseWorld,
    'updateendpoint_iri' : UKPowerSystemBaseWorld}

"""ONS in Blazegraph"""
# ONS = {
#     'label': 'ons',
#     'type':'ORIGINAL',
#     'endpoint_iri' : "http://statistics.data.gov.uk/sparql.json",
#     'queryendpoint_iri' : "http://statistics.data.gov.uk/sparql.json",
#     'updateendpoint_iri' : "http://statistics.data.gov.uk/sparql.json"}

## Local ONS KG deployed in the digital ocean 
ONS = {
    'label': 'ons',
    'type':'DIGITALOCEAN',
    'endpoint_iri' : ONSEndpoint,
    'queryendpoint_iri' : ONSEndpoint,
    'updateendpoint_iri' : ONSEndpoint}

"""UK population data"""
UKPopulationData = {
    'label': 'UKPopulationData',
    'endpoint_iri' : UKPopulationData,
    'queryendpoint_iri' : UKPopulationData,
    'updateendpoint_iri' : UKPopulationData}


