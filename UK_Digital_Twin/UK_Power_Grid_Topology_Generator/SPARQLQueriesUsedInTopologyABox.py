##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 10 May 2021          #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Topology A-boxes"""

def queryBusGPS(graph, FromBus_iri, ToBus_iri):
    queryStr = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT ?FromBus_latitude ?FromBus_longitude ?ToBus_latitude ?ToBus_longitude 
    WHERE
    {
    <%s> space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_FromBus .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_FromBus .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_FromBus .
    ?x_coordinate_FromBus  system:hasValue ?GPS_x_coordinate_FromBus .
    ?y_coordinate_FromBus  system:hasValue ?GPS_y_coordinate_FromBus . 
    ?GPS_x_coordinate_FromBus  system:numericalValue ?FromBus_latitude .
    ?GPS_y_coordinate_FromBus  system:numericalValue ?FromBus_longitude .
    
    <%s> space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_ToBus .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_ToBus .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_ToBus .
    ?x_coordinate_ToBus  system:hasValue ?GPS_x_coordinate_ToBus .
    ?y_coordinate_ToBus  system:hasValue ?GPS_y_coordinate_ToBus . 
    ?GPS_x_coordinate_ToBus  system:numericalValue ?ToBus_latitude .
    ?GPS_y_coordinate_ToBus  system:numericalValue ?ToBus_longitude .
    }
    """ % (FromBus_iri, ToBus_iri)
    qres = graph.query(queryStr)
    return qres

def queryBusLocation(ConjunctiveGraph):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT DISTINCT ?Bus_node ?Location_region
    WHERE
    {
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontocape_upper_level_system:hasAddress ?Location_region . 
    
    GRAPH ?g { ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .}
    
    }
    """
    qres = ConjunctiveGraph.query(queryStr)
    return qres