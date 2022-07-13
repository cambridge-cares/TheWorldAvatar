##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 12 July 2022         #
##########################################

"""This module is designed to generate and update the A-box of UK population density graph."""
import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, RDFS
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import PopulationDensityDataProperty as PopulationData
from UK_Digital_Twin_Package import UKPopulationDensity as UKPD
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
import uuid
from logging import raiseExceptions


"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'
TTL = '.ttl'

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class UKPopulationDensity"""
ukpd = UKPD.UKPopulationDensity()

"""OWL file storage path"""
defaultStoredPath = ukpd.StoreGeneratedOWLs

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontoSDG                         = owlready2.get_ontology(t_box.ontoSDG).load()
ontoenergysystem                = owlready2.get_ontology(t_box.ontoenergysystem).load()

### Functions ### 
""" Create the PopulationDensityDataProperty Instance by specifying its version """
def createPopulationDensityDataProperty(version):
    populationdata = PopulationData.PopulationDensityDataProperty(version)
    data = readFile(populationdata.PopulationDensityDataPath)
    header = populationdata.headerPopulationDensityData
    return data, header

"""Main Function: Add Triples to the named graph"""
def addUKPopulationDensityTriples(version, OWLFileStoragePath = None, updateLocalOWLFile = True, storeType = 'default'):  
    print(defaultStoredPath)
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return
    store = LocalGraphStore(storeType)   
    
    data, header = createPopulationDensityDataProperty(version)    
    
    if len(data[0]) != len(header):
        raise Exception("The population density data header does not match!!")
    else:
        del data[0]

    ## Create rdf graph with identifier
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    graph = Graph(store = store, identifier = URIRef(ontologyIRI))
    
    ## Import T-boxes and add attributes of the ontology
    graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
    graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoSDG)))
    graph.set((graph.identifier, RDFS.comment, Literal('This ontology represents population density of Great Britain.')))
    graph.set((graph.identifier, RDFS.label, Literal('UK Digital Twin - Population Density - Great Britain - Year of ' + str(version))))
    
    print("################################Start adding population density information################################")
    for pd in data:
        print("This is ", data.index(pd), "out of ", len(data))
        LocationPointIRI = dt.baseURL + SLASH + t_box.ontosdgName + SLASH + ukpd.LocationKey + str(uuid.uuid4())
        PopulationIRI = dt.baseURL + SLASH + t_box.ontosdgName + SLASH + ukpd.PopulationKey + str(uuid.uuid4())
        valueOfPopulationIRI = dt.baseURL + SLASH + t_box.ontosdgName + SLASH + ukpd.valueKey + str(uuid.uuid4())

        latlon = str(pd[0][1:len(pd[0])-1] + '#' + pd[1][1:len(pd[1])-1]).replace('\xa0', '')

        graph.add((URIRef(LocationPointIRI), RDF.type, URIRef(ontoSDG.Location.iri)))
        graph.add((URIRef(LocationPointIRI), URIRef(ontoenergysystem.hasWGS84LatitudeLongitude.iri), \
                    Literal(latlon, datatype = 'http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon')))
        graph.add((URIRef(LocationPointIRI), URIRef(ontoSDG.hasPopulation.iri), URIRef(PopulationIRI)))
        graph.add((URIRef(PopulationIRI), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(valueOfPopulationIRI)))
        graph.add((URIRef(valueOfPopulationIRI), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(valueOfPopulationIRI),  URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(pd[2][1:len(pd[2])-2]))))
             
   
        # print(graph.serialize(format="turtle").decode("utf-8"))
            
    ## generate/update OWL files
    if updateLocalOWLFile == True:
        # Store/update the generated owl files      
        if filepath[-1:] != '\\': 
            filepath_ = filepath + '\\' + 'GB_Population_Density_' + str(version) + TTL
        else:
            filepath_ = filepath + 'GB_Population_Density_' + str(version) + TTL
        
        storeGeneratedOWLs(graph, filepath_)
    return

if __name__ == '__main__':
    addUKPopulationDensityTriples(2019, None, True, 'default')


        
    