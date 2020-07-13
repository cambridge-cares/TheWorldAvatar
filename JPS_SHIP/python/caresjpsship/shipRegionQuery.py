'''
module that retreives and pack adms input info
'''
import rdflib
import requests

import math
import sys
import os
import json
import rdflib.plugins.sparql.results.jsonresults as jsresult
from collections import namedtuple
from pyproj import Proj, transform




class shipRegionQuery(object):
   

    def __init__(self, endpoint, connectType, numLimit, xmin, ymin, xmax, ymax):
        '''constructor
        inputs:
        range - user input range {'xmin', 'xmax', 'ymin', 'ymax'}, actual range is the min(user range, region envelope(e.g. jurongisland))
        topnode - uri of topnode to begin search within tree struct,
        filtersrc - true if use all children under topnode as src, false to use topnode as src directly
        bdnnode - top/colleciton node of building
        pollutants: pollutant to test
        srcLimit: limit of src number, actual number might fewer
        bdnLimit: limit of bdn number, actual number might fewer
        '''
        self.numLimit = numLimit
        self.endpoint = endpoint
        self.connectType = connectType 
        coordC = defineCoordConvert('epsg:3857','epsg:4326')
        lower = coordC(float(xmin), float(ymin))
        upper = coordC(float(xmax), float(ymax))
        # print(lower)
        # print(upper)
        xlow = min(lower[0], upper[0])   
        xupper = max(lower[0], upper[0])
        ylow = min(lower[1], upper[1])
        yupper =  max(lower[1], upper[1])
        self.range = (xlow, ylow, xupper, yupper)

        self.address = None
        # print(self.range)




    def queryRange(self):
        '''filter the source from tree starting from topnode, within the range and with user set content
        returns: list of source uris
        ''' 


        self.connectDB(self.endpoint,self.connectType)
        qstr ='''
        PREFIX sys: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX owl: <http://www.w3.org/2002/07/owl#>
        PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
        PREFIX material: <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#>

        SELECT DISTINCT ?a ?x ?y 
        WHERE {{
        ?a a <http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#Ship>.
        ?a space_and_time_extended:hasGISCoordinateSystem ?o.
        ?o  space_and_time_extended:hasProjectedCoordinate_x ?xe.
        ?xe sys:hasValue ?vx.
        ?vx sys:numericalValue ?x .



        OPTIONAL{{
        ?o  space_and_time_extended:hasProjectedCoordinate_y ?ye.
        ?ye sys:hasValue ?vy.
        ?vy sys:numericalValue ?y .
        }}
        }}
        '''.format(self.endpoint)
        #print (qstr)
        coordQresults = self.query(qstr)

  
        filtered = []
            ##filter children within range
        for row in coordQresults:
            x,y,auri = float(row['x'].toPython()), float(row['y'].toPython()),row['a'].toPython()
            #print("{},{},{}".format(x, y, content))
            if  x - self.range[0]>=0 and x - self.range[2] <= 0 and y - self.range[1] >= 0 and y - self.range[3]<= 0:  
                filtered.append(auri)
                #print('add to filtered {}'.format(auri))
    
        return filtered
    
  



    def queryEndpoint(self, str):
        def literal2TLit(sparqlres):

            if 'results' not in sparqlres:
                return
            for row in sparqlres['results']['bindings']:
                for name,value in row.items():
                    if value['type'] == 'literal' and 'datatype' in value:
                        value['type'] = 'typed-literal'

        #print('requesting @ '+self.address+" with query:")
        #print(str)
        resp = requests.get(self.address, params = {'query':str}, timeout = 1500, headers = {'user-agent': 'my-app/0.0.1'})
        #print('raw resp:')

        #print(resp.json())
        result = resp.json()
        literal2TLit(result)
        #print('literal to typed-literal')
        #print(result)
        qres = jsresult.JSONResult(result)#json decoded
        #print('after parse:')
        #print(qres)
        return qres


    
    def queryLocalGraph(self, str): 
        qres = self.g.query(str)
        return qres

    def Uri2Local(uri):
        '''replace a uri to local address
        inputs:
        uri - uri to be changed
        returns: string - local address
        '''
        return uri.replace("http://www.jparksimulator.com",config.root ).replace("http://www.theworldavatar.com",config.root)

    def remote2local(self, func):
        '''decorator to change connection function to local connection by replacing given iri to local address
        '''
        def functionWrapper(self, address):
            address = self.Uri2Local(address)
            func(self, address)
        return functionWrapper    
    
    def connectDB(self, address, connectType = 'endpoint'):
        '''connect to db anyhow (we use rdflib graph parse now)
        '''
        def connectDBActual(address):
            '''
            Actual method to connect to db
            '''
            #obsolete: use rdflib locally
            self.address = address
            if connectType is 'parse':
                self.g = rdflib.Graph()#comment out in future
                self.g.parse(address)#comment out in future


        self.qmethodMap = {'parse': self.queryLocalGraph, 'endpoint':self.queryEndpoint}

        if not sameGraph(address, self.address):
            #print ('parsing graph: '+ address)
            if connectType not in self.qmethodMap:
                raise exception('db connection method not defined')
            #self.connectType = connectType
            self.query = self.qmethodMap[connectType]
            connectDBActual(address)                 
       

        
               


def sameGraph(uri1, uri2):
    def trimloc(uri):
        if uri is None:
            return None
        else:
            return uri.split('#')[0]
    return trimloc(uri1) == trimloc(uri2)

def defineCoordConvert(inCode, outCode):
    inProj = Proj(init=inCode)
    outProj = Proj(init=outCode)
    def coordConvert(x,y):
        return transform(inProj, outProj, x, y)
    return coordConvert

def uri2name(uri):
    base = 'http://www.theworldavatar.com/'
    return uri.split('#')[1]


if __name__ == "__main__":
    q  = shipRegionQuery(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5],sys.argv[6],sys.argv[7])
    res = q.queryRange()
    pythonObj = {}
    pythonObj["shipIRIs"] = res
    print(json.dumps(pythonObj))
