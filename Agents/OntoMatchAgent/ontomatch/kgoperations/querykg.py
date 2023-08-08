from rdflib import BNode, Literal, URIRef

from ontomatch.kgoperations.javagateway import jpsBaseLibGW
import json
import re

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def querykg(sparqlEndPoint=None, queryStr=None):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    StoreRouter = jpsBaseLib_view.StoreRouter
    StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    response = json.loads(str(StoreClient.executeQuery(queryStr)))
    return response

def updatekg(sparqlEndPoint=None, updateStr=None):
    StoreRouter = jpsBaseLib_view.StoreRouter
    StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, False, True)
    response = json.loads(str(StoreClient.executeUpdate(updateStr)))
    return response

#Dummy function for local test
def res2triples(reslines):
    triples = []

    #determine types
    for b in reslines:
        objectType = inferObjectType(b['Object'])
        if objectType == 'literal':
            v = b['Object']
            #if v.replace('.', '', 1).isdigit():#check if numerical value
            #    if '.' in v:
            #        v = float(v)
            #    else:
            #        v = int(v)
            object = Literal(v)
        elif objectType == 'bnode':
            object = BNode(b['Object'])
        else:
            object = URIRef(b['Object'])

        if inferObjectType(b['Subject']) == 'bnode':
            sub = BNode(b['Subject'])
        else:
            sub = URIRef(b['Subject'])
        triples.append((sub, URIRef(b['Predicate']), object))
    return triples

def inferObjectType(objectStr):
    isBNode = re.search('^[a-z0-9]{32}$', objectStr)
    isIRI = re.search('^https*:\/\/.+$', objectStr)
    if isBNode:
        return 'bnode'
    elif isIRI:
        return 'iri'
    else:
        return 'literal'
