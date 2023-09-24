"""This module is designed to generate and update the A-box of UK population density graph."""
import os, json
import owlready2
import math
import pandas as pd
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, RDFS
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKPopulationDensity as UKPD
from UK_Digital_Twin_Package import PopulationDensityDataProperty as PopulationData
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs
import uuid
from pyderivationagent.kg_operations.sparql_client import PySparqlClient

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
ukpd = UKPD.UKPopulationDensity()

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontoSDG                         = owlready2.get_ontology(t_box.ontoSDG).load()
ontoenergysystem                = owlready2.get_ontology(t_box.ontoenergysystem).load()

### Functions ### 
""" Create the PopulationDensityDataProperty Instance by specifying its version """
def createPopulationDensityDataProperty(version, pointDistance):
    populationdata = PopulationData.PopulationDensityDataProperty(version, pointDistance)
    data = read_single_csv(populationdata.PopulationDensityDataPath)
    header = populationdata.headerPopulationDensityData
    return data, header

def read_single_csv(input_path):
    print('...The file %s starts loading...'%input_path)
    df_chunk=pd.read_csv(input_path,chunksize=1000)
    res_chunk=[]
    for chunk in df_chunk:
        res_chunk.append(chunk)
    res_df=pd.concat(res_chunk)
    res_df = res_df.values.tolist()
    print('---The file %s has been successfully loaded.---'%input_path)
    return res_df
    
"""Main Function: Add Triples to the named graph"""
def addUKPopulationDensityTriples(version, limitLines, pointDistance, updateEndpointIRI, KGFileStoragePath, updateLocalOWLFile = True):  
    data, _ = createPopulationDensityDataProperty(version, pointDistance)
    # data = read_single_csv(dataPath)
    print("################################Start adding population density information################################")
    numberOfFiles = math.ceil(len(data) / int(limitLines))
    print('...The total number of population density TLL file is %s...'% str(numberOfFiles))

    for i in range(0, int(numberOfFiles)):
        ## Create rdf graph with identifier
        ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
        graph = Graph(store = 'default', identifier = URIRef(ontologyIRI))
        
        ## Import T-boxes and add attributes of the ontology
        graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
        graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoSDG)))
        graph.set((graph.identifier, RDFS.comment, Literal('This ontology represents population density of Great Britain.')))
        graph.set((graph.identifier, RDFS.label, Literal('UK Digital Twin - Population Density - Great Britain - Year of ' + str(version))))
   
        if i == numberOfFiles - 1:
            upperLimit = int(len(data))
        else:
            upperLimit = int((i + 1) * limitLines)

        for k in range(int(i * limitLines), upperLimit):
            pd  = data[k]
            print("This is " + str(data.index(pd)) + " out of " + str(len(data)))
            LocationPointIRI = dt.baseURL + SLASH + t_box.ontosdgName + SLASH + ukpd.LocationKey + str(uuid.uuid4())
            PopulationIRI = dt.baseURL + SLASH + t_box.ontosdgName + SLASH + ukpd.PopulationKey + str(uuid.uuid4())
            valueOfPopulationIRI = dt.baseURL + SLASH + t_box.ontosdgName + SLASH + ukpd.valueKey + str(uuid.uuid4())
            
            latlon = str(pd[0]) + '#' + str(pd[1])

            graph.add((URIRef(LocationPointIRI), RDF.type, URIRef(ontoSDG.Location.iri)))
            graph.add((URIRef(LocationPointIRI), URIRef(ontoenergysystem.hasWGS84LatitudeLongitude.iri), \
                        Literal(latlon, datatype = 'http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon')))
            graph.add((URIRef(LocationPointIRI), URIRef(ontoSDG.hasPopulation.iri), URIRef(PopulationIRI)))
            graph.add((URIRef(PopulationIRI), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(valueOfPopulationIRI)))
            graph.add((URIRef(valueOfPopulationIRI), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(valueOfPopulationIRI),  URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(pd[2]))))     
    
        ## generate/update OWL files
        if updateLocalOWLFile == True:
            # Store/update the generated owl files      
            if KGFileStoragePath[-1:] != '/': 
                filepath_ = KGFileStoragePath + '/' + 'GB_Population_Density_' + str(version) + '_' + str(pointDistance) + 'km_' + str(i) + TTL
            else:
                filepath_ = KGFileStoragePath + 'GB_Population_Density_' + str(version) + '_' + str(pointDistance) + 'km_' + str(i) + TTL
            
            storeGeneratedOWLs(graph, filepath_)
            print('File Saved')
        
            sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
            sparql_client.uploadOntology(filepath_)
    return

        
    