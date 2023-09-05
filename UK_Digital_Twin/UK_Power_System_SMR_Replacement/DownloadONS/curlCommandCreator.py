##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 15 August 2023       #
##########################################

"""This module is designed to create the curl command for downloading the triples"""
import math, json
import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, BASE) 
from UK_Digital_Twin_Package.queryInterface import performQuery
from pyderivationagent.kg_operations.sparql_client import PySparqlClient

def curlCommandCreator(totalNumberOfTriples:int, interval:int, queryEndpoint:str, constructStr:str, fileName:str, batFileSavedPath:str, removedLines:int = 0):
    if interval > totalNumberOfTriples:
        raise ValueError("The interval must be less or equal to the total number of triples.")

    numOfFile = math.ceil(totalNumberOfTriples/interval)

    curlstr = """"""

    for i in range(numOfFile):
        if i >= removedLines:
            if i == numOfFile - 1:
                lastLimit = totalNumberOfTriples - (i * interval)
                curlstr += """curl -X POST "%s" --data-urlencode "query=CONSTRUCT { %s } WHERE { %s } LIMIT %s OFFSET %s" -H "Accept:text/turtle" | gzip > %s.gz"""%(queryEndpoint, constructStr, constructStr, str(lastLimit), str(i * interval), fileName + str((i + 1) * interval))
            else:
                curlstr += """curl -X POST "%s" --data-urlencode "query=CONSTRUCT { %s } WHERE { %s } LIMIT %s OFFSET %s" -H "Accept:text/turtle" | gzip > %s.gz \n"""%(queryEndpoint, constructStr, constructStr, str(interval), str(i * interval), fileName + str((i + 1) * interval))

    batwriter = open(batFileSavedPath + fileName + '.bat','w')
    batwriter.write(curlstr)
    batwriter.close()
    return 

def curlQueryFOROBSOLETE(LACodeList:list, queryEndpoint:str, fileName:str, batFileSavedPath:str):
    curlstr = """"""
    for i, lacode in enumerate(LACodeList):
        id = "<http://statistics.data.gov.uk/id/statistical-geography/" + str(lacode) + ">"
        strq = """ %s <http://publishmydata.com/def/ontology/foi/code> ?LACODE .  %s <http://statistics.data.gov.uk/def/statistical-entity#code> ?code . """%(id, id)
        curlstr += """curl -X POST "%s" --data-urlencode "query=CONSTRUCT { %s } WHERE { %s } " -H "Accept:text/turtle" | gzip > %s.gz \n"""%(queryEndpoint, strq, strq, fileName + str(i))

    batwriter = open(batFileSavedPath + fileName + '.bat','w')
    batwriter.write(curlstr)
    batwriter.close()
    return 

def curlQueryFORWITHIN_withClearPath(pathList:list, queryEndpoint:str, fileName:str, batFileSavedPath:str):
    curlstr = """"""
    for p in pathList:
        for i in range(len(p)):
            if not '<' in p[i]:
                p[i] = "<" + p[i] + ">"
    for j, p in enumerate(pathList):
        if len(p) == 2:
            qureyCLAUSE =""" %s <http://publishmydata.com/def/ontology/foi/within> %s . %s <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.data.gov.uk/id/statistical-entity/E12> . %s <http://publishmydata.com/def/ontology/foi/code> ?LACode_Region . """%(p[0], p[1], p[1], p[1])
        elif len(p) == 3:
            qureyCLAUSE =""" %s <http://publishmydata.com/def/ontology/foi/within> %s . %s <http://publishmydata.com/def/ontology/foi/within> %s . %s <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.data.gov.uk/id/statistical-entity/E12> . %s <http://publishmydata.com/def/ontology/foi/code> ?LACode_Region . """%(p[0], p[1], p[1], p[2], p[2], p[2])
      
        curlstr += """curl -X POST "%s" --data-urlencode "query=CONSTRUCT { %s } WHERE { %s } " -H "Accept:text/turtle" | gzip > %s.gz \n"""%(queryEndpoint, qureyCLAUSE, qureyCLAUSE, fileName + str(j))
        
    batwriter = open(batFileSavedPath + fileName + '.bat','w')
    batwriter.write(curlstr)
    batwriter.close()
    return

def findInterStepsAmongWITHIN(LACodeList:list, querEndpoint:str):

    res_record = []
    cannotFindInTwoLayer_LAcodeList = []

    if not 'json' in querEndpoint:
        querEndpoint = "http://statistics.data.gov.uk/sparql.json"
    for lacode in LACodeList:
        loopFound_flag = False
        areaIRI = "<http://statistics.data.gov.uk/id/statistical-geography/" + str(lacode) + ">"
        qstr = """
                SELECT DISTINCT ?Region ?code
                WHERE{
                %s <http://publishmydata.com/def/ontology/foi/within> ?Region . 
                ?Region <http://statistics.data.gov.uk/def/statistical-entity#code> ?code. 
                }
                """%areaIRI

    
        res = json.loads(performQuery(querEndpoint, qstr)) 
        for r in res:
            if r['code'][-3:] == 'E12': # http://statistics.data.gov.uk/id/statistical-entity/
                res_record.append([areaIRI, r['Region']])
                loopFound_flag = True
                break
        
        ## Search for the second layer
        if loopFound_flag is False:
            temp_result = []
            for r in res:
                if 'E' in r['code'][-3:]:
                    qstr = """
                    SELECT DISTINCT ?Region ?code
                    WHERE{
                    %s <http://publishmydata.com/def/ontology/foi/within> %s . 
                    %s <http://publishmydata.com/def/ontology/foi/within> ?Region . 
                    ?Region <http://statistics.data.gov.uk/def/statistical-entity#code> ?code. 
                    }
                    """%(areaIRI, "<" + r['Region'] + ">", "<" + r['Region'] + ">")
                
                    res_r = json.loads(performQuery(querEndpoint, qstr)) 
                    for r_ in res_r:
                        if r_['code'][-3:] == 'E12': # http://statistics.data.gov.uk/id/statistical-entity/  
                            res_record.append([areaIRI, "<" + r['Region'] + ">", "<" + r_['Region'] + ">"])
                            loopFound_flag = True
                            break
                        elif 'E' in r_['code'][-3:]:
                            temp_result.append([areaIRI, "<" + r['Region'] + ">", "<" + r_['Region'] + ">"])

                    if loopFound_flag == True:
                        break
        
        if loopFound_flag is False:
            cannotFindInTwoLayer_LAcodeList.append(lacode)

    return res_record, cannotFindInTwoLayer_LAcodeList

def findWITHINK03(LACodeList:list, queryEndpoint:str, fileName:str, batFileSavedPath:str):
    curlstr = """"""
    for i, lacode in enumerate(LACodeList):
        id = "<http://statistics.data.gov.uk/id/statistical-geography/" + str(lacode) + ">"
        strq = """ %s <http://publishmydata.com/def/ontology/foi/code> ?LACODE . %s <http://publishmydata.com/def/ontology/foi/within> ?WITHINPLACE . ?WITHINPLACE <http://publishmydata.com/def/ontology/foi/code> ?LACODEWITHINPLACE . ?WITHINPLACE <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.data.gov.uk/id/statistical-entity/K03> . """%(id, id)
        curlstr += """curl -X POST "%s" --data-urlencode "query=CONSTRUCT { %s } WHERE { %s } " -H "Accept:text/turtle" | gzip > %s.gz \n"""%(queryEndpoint, strq, strq, fileName + str(i))

    batwriter = open(batFileSavedPath + fileName + '.bat','w')
    batwriter.write(curlstr)
    batwriter.close()
    return 

def queryLocalCode(startTime_of_EnergyConsumption, UKDigitalTwinEndPoint_iri):    
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>    
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ont: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?Area_LACode
    WHERE 
    {   
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    
    ?Total_ele_consumption ontoenergysystem:isObservedIn/ontoenergysystem:hasLocalAuthorityCode ?Area_LACode .
    }
    """% (startTime_of_EnergyConsumption)
    
    print('...Query ElectricityConsumption_LocalArea...')
    res = json.loads(performQuery(UKDigitalTwinEndPoint_iri, queryStr)) 
    print('...Query ElectricityConsumption_LocalArea is done...')
     
    for r in res:
        for key in r.keys():
            if '\"^^' in  r[key] :
                r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
    LAList = []

    ## Only LA code within England
    # for r in res:
    #     if r['Area_LACode'][0] == 'E' and not int(r['Area_LACode'][1] + r['Area_LACode'][2]) >= 11:
    #         LAList.append(r['Area_LACode'])      


    ## Eliminate the North Ireland and LA code represents the 
    for r in res:
        if r['Area_LACode'][0] != 'K' and r['Area_LACode'][0] != 'N':
            LAList.append(r['Area_LACode'])   

    return LAList          

def uploadONSFile(numOfFile:int, filePath:str, fileName:str, extension:str, stratNum:int, interval:int, updateEndpointIRI:str, givenIndexList:[]):
    if filePath[-1:] != "/": 
        filePath = filePath + '/'
    if extension[0] != ".":
       extension = extension + "."

    if len(givenIndexList) != 0:
        for index in givenIndexList:
            filepath_ = filePath + fileName + str(index) + extension
            # os.rename(filepath_, filepath_ + extension)
            sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
            sparql_client.uploadOntology(filepath_) # + extension)
    else:
        for i in range(numOfFile):
            filepath_ = filePath + fileName + str(stratNum + interval * i) # + extension
            os.rename(filepath_, filepath_ + extension)
            sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
            sparql_client.uploadOntology(filepath_ + extension)

    print('--The upload of %s is done.--'%fileName)
    return 

if __name__ == '__main__':

    ### QUERY Area boundaries and LA code ###
    # totalNumberOfTriples = 324832
    # interval = 1000
    # queryEndpoint = "https://statistics.data.gov.uk/sparql"
    # constructStr = """?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> . ?s <http://publishmydata.com/def/ontology/foi/code> ?LACode . ?s <http://www.opengis.net/ont/geosparql#hasGeometry> ?geometry . ?geometry <http://www.opengis.net/ont/geosparql#asWKT> ?areaBoundary ."""
    # fileName = "onsareaboundariesandlacode"
    # batFileSavedPath = "/mnt/d/wx243/FromTWA/ONSBatFile/" 

    ### QUERY within ###
    # StartingOFFSET = 49550000
    # totalNumberOfTriples = 61237017
    # interval = 50
    # queryEndpoint = "https://statistics.data.gov.uk/sparql"
    # constructStr = """?s <http://publishmydata.com/def/ontology/foi/within> ?place . """
    # fileName = "onswithinRelationship"
    # batFileSavedPath = "/mnt/d/wx243/FromTWA/ONSBatFile/" 

    # ### QUERY code ###
    # totalNumberOfTriples = 506928
    # interval = 50000
    # queryEndpoint = "https://statistics.data.gov.uk/sparql"
    # constructStr = """?s <http://statistics.data.gov.uk/def/statistical-entity#code> ?code . """
    # fileName = "onsareacode"
    # batFileSavedPath = "/mnt/d/wx243/FromTWA/ONSBatFile/" 

    # curlCommandCreator(totalNumberOfTriples, interval, queryEndpoint, constructStr, fileName, batFileSavedPath, 991000)

    # ### QUERY obsolete code ###
    # LACodeList = ["E07000150", "E07000151", "E07000152", "E07000153", "E07000154", "E07000155", "E07000156"]
    # queryEndpoint = "https://statistics.data.gov.uk/sparql"
    # fileName = "onsOBSOLETE"
    # batFileSavedPath = "/mnt/d/wx243/FromTWA/ONSBatFile/" 

    # curlQueryFOROBSOLETE(LACodeList, queryEndpoint, fileName, batFileSavedPath)


    ###-- The new within sub set --##
    # queryEndpoint = "https://statistics.data.gov.uk/sparql"
    # fileName = "ONSwithinSubset"
    # batFileSavedPath = "/mnt/d/wx243/FromTWA/ONSBatFile/"
    # # queryStr = " <http://publishmydata.com/def/ontology/foi/within>+ ?Region . ?Region <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.data.gov.uk/id/statistical-entity/E12> . ?Region <http://publishmydata.com/def/ontology/foi/code> ?LACode_Region ."
    # lacodelist = queryLocalCode("2017-01-31", "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql")
    # res_record, cannotFindInTwoLayer_LAcodeList = findInterStepsAmongWITHIN(lacodelist, queryEndpoint)
    # curlQueryFORWITHIN_withClearPath(res_record, queryEndpoint, fileName, batFileSavedPath)
    # # curlQueryFORWITHIN(queryStr, lacodelist, queryEndpoint, fileName, batFileSavedPath)


    ###-- upload the area bounderies and LA code --###
    # filePath = "/mnt/d/wx243/FromTWA/ONS_KG/ONSKG_AreaBoundariesAndLACode/"
    # fileName = "onsareaboundariesandlacode"
    # numOfFile = 344
    # stratNum = 1000
    # interval = 1000
    # updateEndpointIRI = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ONS_subset/sparql"
    # givenIndexList = [219500, 219750, 280250, 280375, 280500, 281125, 281190, 281250, 281500, 281625, 281750, 317500, 318500, 319500, 321500, 322500, 323500]


    ##-- upload the ONSKG_WithinRelations --## 
    # filePath = "/mnt/d/wx243/FromTWA/ONS_KG/ONSKG_WithinRelations/"
    # fileName = "ONSwithinSubset"
    # numOfFile = 309
    # stratNum = 0
    # interval = 1
    # updateEndpointIRI = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ONS_subset/sparql"
    # givenIndexList = []

    ##-- upload the ONSKG_WithinRelations --## 
    # filePath = "/mnt/d/wx243/FromTWA/ONS_KG/ONSKG_AreaCode/"
    # fileName = "onsareacode"
    # numOfFile = 11
    # stratNum = 50000
    # interval = 50000
    # updateEndpointIRI = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ONS_subset/sparql"
    # givenIndexList = []

    ##-- upload the ONSKG_WithinRelations --## 
    # filePath = "/mnt/d/wx243/FromTWA/ONS_KG/ONSObsolete/"
    # fileName = "onsOBSOLETE"
    # numOfFile = 7
    # stratNum = 0
    # interval = 1
    # updateEndpointIRI = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ONS_subset/sparql"
    # givenIndexList = []
    # uploadONSFile(numOfFile, filePath, fileName, '.ttl', stratNum, interval, updateEndpointIRI, givenIndexList)

    ##-- curl the triples describes the areas within K03000001 --##
    # queryEndpoint = "https://statistics.data.gov.uk/sparql"
    # lacodelist = queryLocalCode("2017-01-31", "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql")
    # filePath = "/mnt/d/wx243/FromTWA/ONSBatFile/"
    # findWITHINK03(lacodelist, queryEndpoint, "withinGB", filePath)

    ##-- upload the ONSKG_WithinK03000001 (GB) --## 
    filePath = "/mnt/d/wx243/FromTWA/ONS_KG/WithinGB/"
    fileName = "withinGB"
    numOfFile = 392
    stratNum = 0
    interval = 1
    updateEndpointIRI = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ONS_subset/sparql"
    givenIndexList = []
    uploadONSFile(numOfFile, filePath, fileName, '.ttl', stratNum, interval, updateEndpointIRI, givenIndexList)
    