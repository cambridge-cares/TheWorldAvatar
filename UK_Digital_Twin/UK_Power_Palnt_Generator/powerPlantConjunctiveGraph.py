##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 10 May 2021          #
##########################################

"""This module is designed to generate a conjunctive graph containing all UK power plant sub-graphs graph catched in an on-disk database named Sleepycat."""
import os
from rdflib import Namespace, Literal, URIRef
from rdflib.graph import Graph, ConjunctiveGraph
# from rdflib.plugins.memory import IOMemory
from rdflib.plugins.sleepycat import Sleepycat
# from rdflib.namespace import NamespaceManager
# from rdflib.store import NO_STORE, VALID_STORE
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
# from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
# from UK_Digital_Twin_Package import DUKESDataProperty as DUKES
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
# from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile

"""Create an object of Class UKPowerPlantDataProperty"""
ukpp = UKpp.UKPowerPlant()

"""Graph store"""
store = Sleepycat()
store.__open = True
store.context_aware = True

"""Create ConjunctiveGraph"""
powerPlantConjunctiveGraph = ConjunctiveGraph(store=store, identifier = ukpp.identifier_powerPlantConjunctiveGraph)

"""Open store"""
powerPlantConjunctiveGraph.open(ukpp.SleepycatStoragePath, create = False)

# numOf_pp = 0
# for q in powerPlantConjunctiveGraph.quads((None, RDF.type, URIRef(ontoeip_powerplant.PowerPlant.iri),None)): 
#         # print(q[0])
#         numOf_pp += 1

print(powerPlantConjunctiveGraph.identifier)

queryStr3 = """
        SELECT DISTINCT ?g
        WHERE 
            {
              graph ?g {?s ?p ?o}
            }    
        """    
qres = powerPlantConjunctiveGraph.query(queryStr3)

# print('The query results are: ')
# for n in qres:
#     print(n)   

num = 0
for n in qres:
    num += 1
print(num)   




"""Close the store"""
powerPlantConjunctiveGraph.close()