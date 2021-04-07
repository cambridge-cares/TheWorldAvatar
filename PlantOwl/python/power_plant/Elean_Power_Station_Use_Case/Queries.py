#########################################################
# Author: Zach, Dave and Feroz Farazi (msff2@cam.ac.uk) #
# Date: 24 Nov 2020                                     #
#########################################################

"""This module defines SPARQL queries"""

def get_lucode_label():
    """Sets the current SPARQL query"""
    query = """
            Prefix ns1:<http://www.theworldavatar.com/kb/ontocropenergy/> 
            Prefix ns2: <http://www.theworldavatar.com/ontology/ontocropenergy/OntoCropEnergy.owl#> 
            Prefix ns3: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> 
            Prefix OLU: <http://www.theworldavatar.com/ontology/ontolanduse/OntoLandUse.owl#> 
            Prefix OCMGML: <http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#> 
                 
            SELECT ?Crop ?Lucode 
            WHERE  
            { 
            ?Crop OCMGML:hasLucode ?Lucode .
                 
            Filter(?Crop=ns1:SpringWheat||?Crop=ns1:WinterWheat||?Crop=ns1:SpringOilseed||?Crop=ns1:WinterOilseed||?Crop=ns1:Miscanthus) 
         
            } 

            """
    return query

def get_crop_energy(crop_iri):
    """Sets the current SPARQL query"""
    query = """
            Prefix ns1:<http://www.theworldavatar.com/kb/ontocropenergy/> 
            Prefix ns2: <http://www.theworldavatar.com/ontology/ontocropenergy/OntoCropEnergy.owl#> 
            Prefix ns3: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> 
            Prefix OLU: <http://www.theworldavatar.com/ontology/ontolanduse/OntoLandUse.owl#> 
            Prefix OCMGML: <http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#> 
 
            SELECT ?HHV 
            WHERE  
            { 
            <%s> ns2:hasGrossCalorificValue ?HHVRef .
            
            ?HHVRef ns3:value ?HHV .
            } 

            """ % crop_iri
    return query

def get_crop_yield(crop_iri):
    """Sets the current SPARQL query"""
    query = """
            Prefix ns1:<http://www.theworldavatar.com/kb/ontocropenergy/>
            Prefix ns2: <http://www.theworldavatar.com/ontology/ontocropenergy/OntoCropEnergy.owl#>
            Prefix ns3: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
            Prefix OLU: <http://www.theworldavatar.com/ontology/ontolanduse/OntoLandUse.owl#>

            SELECT ?Yield
            WHERE 
            {       
            <%s> ns2:hasCropYield ?YieldRef .
                      
            ?YieldRef ns3:value ?Yield .
            }

            """ % crop_iri
    return query

def get_crop():
    """Sets the current SPARQL query"""
    query = """
            PREFIX geoliteral: <http://www.bigdata.com/rdf/geospatial/literals/v1#>
            PREFIX geo: <http://www.bigdata.com/rdf/geospatial#>
            PREFIX vocabTerm: <http://vocab.datex.org/terms#>
            PREFIX ns2: <http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#>

            SELECT ?Lucode (COUNT(?Lucode) AS ?LucodeTotal) WHERE {
            SERVICE geo:search {
            ?cropMap geo:search "inCircle" .
            ?cropMap geo:predicate vocabTerm:centrePoint .
            ?cropMap geo:searchDatatype geoliteral:lat-lon .
            ?cropMap geo:spatialCircleCenter "52.398370#0.131902" . 
            ?cropMap geo:spatialCircleRadius "96.5606" . # default unit: Kilometers
            ?cropMap geo:locationValue ?locationValue. 
            ?cropMap geo:latValue ?centrePointLatitude .
            ?cropMap geo:lonValue ?centrePointlongitude .
            }
            ?cropMap ns2:hasLucode ?Lucode
            }
            GROUP BY ?Lucode
            ORDER BY DESC(?LucodeTotal)
            """
    return query


def get_use_case_power_plant_iri(power_plant_name):
    power_plant = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/"+power_plant_name+".owl#"+power_plant_name
    query = """
        PREFIX powerplant:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?powerPlantIRI
        WHERE
        {
        ?powerPlantIRI rdf:type powerplant:PowerPlant .
        Filter(?powerPlantIRI = <%s>)
        }
        """ % power_plant
    return query

def get_power_plant_coordinates(power_plant_iri):
    query = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT ?latitude ?longitude
    WHERE
    {
    <%s> space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
    ?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
    ?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
    ?x_coordinate  system:hasValue ?GPS_x_coordinate .
    ?y_coordinate  system:hasValue ?GPS_y_coordinate . 
    ?GPS_x_coordinate  system:numericalValue ?longitude .
    ?GPS_y_coordinate  system:numericalValue ?latitude .
    }
    """ % power_plant_iri
    return query

def get_fuel_type(power_plant_iri):
    query = """
    PREFIX powerplant:  			<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX rdf:        				<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT ?fuel
    WHERE
    {
    <%s> technical_system:realizes ?Generation_Type .
    ?Generation_Type powerplant:consumesPrimaryFuel ?fuel .
    }
    """ % power_plant_iri
    return query

def get_capacity(power_plant_iri):
    query = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
    SELECT ?capacity ?capacity_units
    WHERE
    {
    <%s> system_realization:designCapacity ?Capacity .
    ?Capacity system:hasValue ?Capacity_value .
    ?Capacity_value system:numericalValue ?capacity .
    ?Capacity_value system:hasUnitOfMeasure ?capacity_units .
    }
    """ % power_plant_iri
    return query

def get_generation(power_plant_iri):
    query = """
    PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT DISTINCT ?value_of_AnnualGeneration ?unit_of_AnnualGeneration
    WHERE
    {
    <%s> technical_system:realizes ?Generation_Type .
    ?Generation_Type powerplant:hasAnnualGeneration ?AnnualGeneration .
    ?AnnualGeneration system:hasValue ?AnnualGeneration_value .
    ?AnnualGeneration_value system:numericalValue ?value_of_AnnualGeneration .
    ?AnnualGeneration_value system:hasUnitOfMeasure ?unit_of_AnnualGeneration .
    }
    """ % power_plant_iri
    return query

# def get_lucode_count(circleCentre, lucode):
#     query = """
#             PREFIX geoliteral: <http://www.bigdata.com/rdf/geospatial/literals/v1#>
#             PREFIX geo: <http://www.bigdata.com/rdf/geospatial#>
#             PREFIX vocabTerm: <http://vocab.datex.org/terms#>
#             PREFIX ns2: <http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#>
#             PREFIX ontolanduse: <http://www.theworldavatar.com/kb/ontolanduse/>
#
#             SELECT ?Lucode (COUNT(?Lucode) AS ?LucodeTotal) WHERE {
#             SERVICE geo:search {
#             ?cropMap geo:search "inCircle" .
#             ?cropMap geo:predicate vocabTerm:centrePoint .
#             ?cropMap geo:searchDatatype geoliteral:lat-lon .
#             ?cropMap geo:spatialCircleCenter %s  .
#             ?cropMap geo:spatialCircleRadius "96.5606" . # default unit: Kilometers
#             ?cropMap geo:locationValue ?locationValue.
#             ?cropMap geo:latValue ?centrePointLatitude .
#             ?cropMap geo:lonValue ?centrePointlongitude .
#             }
#             ?cropMap ns2:hasLucode ?Lucode .
#             Filter(?Lucode = ontolanduse:%s)
#             }
#             GROUP BY ?Lucode
#             """ % (circleCentre, lucode)
#     return query

def get_lucode_count(circleCentre):
    query = """
            PREFIX geoliteral: <http://www.bigdata.com/rdf/geospatial/literals/v1#>
            PREFIX geo: <http://www.bigdata.com/rdf/geospatial#>
            PREFIX vocabTerm: <http://vocab.datex.org/terms#>
            PREFIX ns2: <http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#>
            PREFIX ontolanduse: <http://www.theworldavatar.com/kb/ontolanduse/>

            SELECT ?Lucode (COUNT(?Lucode) AS ?LucodeTotal) WHERE {
            SERVICE geo:search {
            ?cropMap geo:search "inCircle" .
            ?cropMap geo:predicate vocabTerm:centrePoint .
            ?cropMap geo:searchDatatype geoliteral:lat-lon .
            ?cropMap geo:spatialCircleCenter %s  .
            ?cropMap geo:spatialCircleRadius "96.5606" . # default unit: Kilometers
            ?cropMap geo:locationValue ?locationValue. 
            ?cropMap geo:latValue ?centrePointLatitude .
            ?cropMap geo:lonValue ?centrePointlongitude .
            }
            ?cropMap ns2:hasLucode ?Lucode .
            }
            GROUP BY ?Lucode
            """ % (circleCentre)
    return query


if __name__=='__main__':
    print(get_generation('http://www.powerplant/iri'))
    print(get_lucode_count("52.4356#0.12343"))