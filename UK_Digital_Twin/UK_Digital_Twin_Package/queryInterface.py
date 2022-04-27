# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# For further information, please check the jps-base_lib, py4jps

###############################################
# Extended by: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 June 2021              #
###############################################

import os, sys
BASE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, BASE)
#from jpsSingletons import jpsBaseLibGW
from py4jps.resources import JpsBaseLib
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

## this function shows how to do a simple KG query
# def performQuery(kb, query, isQuery = True, isUpdate = False):
#     # perform an example sparqle query, see the jps-base-lib docs for further details
#     KGRouter = jpsBaseLib_view.KGRouter
#     print(KGRouter)
#     KGClient = KGRouter.getKnowledgeBaseClient(str(KGRouter.HTTP_KB_PREFIX) + str(kb), isQuery, isUpdate)
#     print(KGClient)
#     if type(KGClient) == 'NoneType':       
#         print('KGClient in the query interfaced has not been created successfully. Please check if the endpoint is already added into the lookup table of Blazegraph.')
#     response = KGClient.executeQuery((query))
#     return str(response)

## TODO: update this function with new RemoteStoreClient.java
def performQuery(kb, query, isQuery = True, isUpdate = False):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.StoreRouter
    KGClient = KGRouter.getStoreClient(str(KGRouter.HTTP_KB_PREFIX) + str(kb), isQuery, isUpdate)
    try:
        response = KGClient.executeQuery((query))
        return str(response)
    except:
        print("KGClient has not been created successfully.")

def performUpdate(kb, query, isQuery = True, isUpdate = True):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.StoreRouter 
    KGClient = KGRouter.getStt(str(KGRouter.HTTP_KB_PREFIX) + str(kb), isQuery, isUpdate)
    try:
        response = KGClient.executeUpdate((query))
        return str(response)
    except:
        print("KGClient has not been created successfully.")

def performFederatedQuery(query, queryendpoints:list):
    # perform an example sparqle query, see the jps-base-lib docs for further details   
    RemoteKnowledgeBaseClient = jpsBaseLib_view.RemoteStoreClient()
    # if len(queryendpoints) == 0:
    #     print('Please specify the remote query endpoints.')
    #     return None    
    # endpoints = []
    # for ed in queryendpoints:        
    #     endpoints.append(str(ed))
    # # RemoteKnowledgeBaseClient.executeFederatedQuery(endpoints, query) 
    try: 
        response = RemoteKnowledgeBaseClient.executeFederatedQuery(list(queryendpoints), query)
        return str(response)
    except:
        print("...RemoteKnowledgeBaseClient has not been created successfully...")

# def performFederatedQuery(query, *queryendpoints):
#     # perform an example sparqle query, see the jps-base-lib docs for further details   
#     RemoteKnowledgeBaseClient = jpsBaseLib_view.RemoteKnowledgeBaseClient()
#     if len(queryendpoints) == 0:
#         print('Please specify the remote query endpoints.')
#         return None    
#     endpoints = []
#     for ed in queryendpoints:        
#         endpoints.append(str(ed))  
#     response = RemoteKnowledgeBaseClient.executeFederatedQuery(endpoints, query)
#     return str(response)





if __name__ == '__main__':  
    queryStr1 = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX dbo: <https://dbpedia.org/ontology/>
    SELECT DISTINCT ?Bus_node ?Location_region ?areacode
    WHERE
    {
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontocape_upper_level_system:hasAddress ?Location_region . 
    
    ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .
    ?Location_region dbo:areaCode ?areacode .
    
    }
    """ 

    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?PowerGenerator ?LACode_PP ?PP_lat_lon ?PrimaryFuel ?GenerationTechnology
    WHERE
    {      	
    ?powerPlant a ontoeip_powerplant:PowerPlant .
    ?powerPlant ontoenergysystem:hasRelevantPlace ?LocatedPlace .
    ?LocatedPlace rdf:type ontoenergysystem:AdministrativeDivision .
    ?LocatedPlace ontoenergysystem:hasLocalAuthorityCode ?LACode_PP .
    
    ?powerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel/rdf:type ?PrimaryFuel .
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology/rdf:type ?GenerationTechnology .  
    
    ?powerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?PP_lat_lon .
    }
    """

    queryONS = """
    PREFIX dcat: <http://www.w3.org/ns/dcat#>
    PREFIX dcterms: <http://purl.org/dc/terms/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX qb: <http://purl.org/linked-data/cube#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX void: <http://rdfs.org/ns/void#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    SELECT *
    WHERE {
    ?s ?p ?o
    }

    LIMIT 100
    """

    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"
    # res = performFederatedQuery(queryStr1, ["https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"])
    ONS_json = "http://statistics.data.gov.uk/sparql.json" 
    # ONS_json = "http://statistics.data.gov.uk/sparql"
    # res = performFederatedQuery(queryStr, [ukdigitaltwinendpoint, ukdigitaltwinendpoint])
    res = performFederatedQuery(queryONS, [ONS_json, ukdigitaltwinendpoint])
    # res = performQuery('ukdigitaltwin_test2', queryStr)
    
    
    print(len(res))
    
    
    
    