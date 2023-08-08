##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 26 July 2022         #
##########################################

"""
This module is used to calculate the population within a given circle 
"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from rfc3987 import parse
from logging import raiseExceptions

def populationDensityCalculator(centre:str, radius, queryEndPointLabel: str) -> float:
    if queryEndPointLabel == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin_pd['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin_pd['endpoint_iri'])
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

    queryStr_geospatial = """
    PREFIX geoliteral: <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo: <http://www.bigdata.com/rdf/geospatial#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ontoSDG: <http://www.theworldavatar.com/ontology/ontosdg/OntoSDG.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
   
    SELECT ?valueOfPopulation 
    WHERE {
    SERVICE geo:search {
        ?Location geo:search "inCircle" .
        ?Location geo:searchDatatype geoliteral:lat-lon .
        ?Location geo:predicate ontoenergysystem:hasWGS84LatitudeLongitude .
        ?Location geo:spatialCircleCenter "%s" .
        ?Location geo:spatialCircleRadius "%s" . # default unit: Kilometers    
    }
        ?Location ontoSDG:hasPopulation ?population .
        ?population  ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?valueOfPopulation .
    }

    """% (centre, str(radius))
    
    print('...perform geospacial query...')
    res = json.loads(performQuery(endPointIRI, queryStr_geospatial))
    print('...geospacial query is done...')

    populationWithinGivenCircle = 0
    for r in res:
        populationWithinGivenCircle += float(r['valueOfPopulation'])
    
    return populationWithinGivenCircle

if __name__ == '__main__':
    p = populationDensityCalculator('52.209556#0.120046', 100, 'ukdigitaltwin_pd')
    print(p)