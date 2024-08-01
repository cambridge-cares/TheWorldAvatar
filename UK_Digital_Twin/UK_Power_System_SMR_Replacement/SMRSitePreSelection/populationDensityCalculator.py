"""
This module is used to calculate the population within a given circle 
"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from rfc3987 import parse
from logging import raiseExceptions
from UK_Digital_Twin_Package.iris import *

def populationDensityCalculator(centre:str, radius, queryEndPointLabel: str) -> float:
    if queryEndPointLabel == str(EndPointConfigAndBlazegraphRepoLabel.UKPopulationData['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPopulationData['endpoint_iri'])
    elif parse(queryEndPointLabel, rule='IRI'):
        endPointIRI = queryEndPointLabel
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")

    if not "#" in centre and type(centre) is list and len(centre) == 2:
        centre = str(centre[0]) + "#" + str(centre[1])
    elif "#" in centre:
        pass 
    else:
        raise ValueError("Cannot calculate population density from centre")

    queryStr_geospatial = f"""
    SELECT ?valueOfPopulation 
    WHERE {{
    SERVICE <{GEO_SEARCH}> {{
        ?Location <{GEO_SEARCH}> "inCircle" .
        ?Location <{GEO_SEARCHDATATYPE}> <{GEOLITERAL_LAT_LON}> .
        ?Location <{GEO_PREDICATE}> <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> .
        ?Location <{GEO_SPATIALCIRCLECENTER}> "{centre}" .
        ?Location <{GEO_SPATIALCIRCLERADIUS}> "{str(radius)}" . # default unit: Kilometers    
    }}
        ?Location <{ONTOSDG_HASPOPULATION}> ?population .
        ?population  <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?valueOfPopulation .
    }}

    """
    print('...perform geospacial query...')
    res = json.loads(performQuery(endPointIRI, queryStr_geospatial))
    print('...geospacial query is done...')

    populationWithinGivenCircle = 0
    for r in res:
        populationWithinGivenCircle += float(r['valueOfPopulation'])
    return populationWithinGivenCircle