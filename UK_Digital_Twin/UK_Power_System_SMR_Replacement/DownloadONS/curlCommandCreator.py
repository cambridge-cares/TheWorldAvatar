##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 August 2023       #
##########################################

"""This module is designed to create the curl command for downloading the triples"""
import math
def curlCommandCreator(totalNumberOfTriples:int, interval:int, queryEndpoint:str, constructStr:str, fileName:str, batFileSavedPath:str):
    if interval > totalNumberOfTriples:
        raise ValueError("The interval must be less or equal to the total number of triples.")

    numOfFile = math.ceil(totalNumberOfTriples/interval)

    curlstr = """"""

    for i in range(numOfFile):
        if i == numOfFile - 1:
            lastLimit = totalNumberOfTriples - (i * interval)
            curlstr += """curl -X POST "%s" --data-urlencode "query=CONSTRUCT { %s } WHERE { %s } LIMIT %s OFFSET %s" -H "Accept:text/turtle" | gzip > %s.gz"""%(queryEndpoint, constructStr, constructStr, str(lastLimit), str(i * interval), fileName + str((i + 1) * interval))
        else:
            curlstr += """curl -X POST "%s" --data-urlencode "query=CONSTRUCT { %s } WHERE { %s } LIMIT %s OFFSET %s" -H "Accept:text/turtle" | gzip > %s.gz \n"""%(queryEndpoint, constructStr, constructStr, str(interval), str(i * interval), fileName + str((i + 1) * interval))

    batwriter = open(batFileSavedPath + fileName + '.bat','w')
    batwriter.write(curlstr)
    batwriter.close()

    return 

if __name__ == '__main__':
    totalNumberOfTriples = 324832
    interval = 5000
    queryEndpoint = "https://statistics.data.gov.uk/sparql"
    constructStr = """?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> . ?s <http://publishmydata.com/def/ontology/foi/code> ?LACode . ?s <http://www.opengis.net/ont/geosparql#hasGeometry> ?geometry . ?geometry <http://www.opengis.net/ont/geosparql#asWKT> ?areaBoundary ."""
    fileName = "onsareaboundariesandlacode"
    batFileSavedPath = "/mnt/d/wx243/FromTWA/ONSBatFile/" 
    curlCommandCreator(totalNumberOfTriples, interval, queryEndpoint, constructStr, fileName, batFileSavedPath)