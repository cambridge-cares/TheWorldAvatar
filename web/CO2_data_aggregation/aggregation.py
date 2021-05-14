
# coding: utf-8

# In[1]:

# use rdflib to parse ontology
# rdflib is a python package working with rdf, supporting SPARQL Queries 

from rdflib import Graph
g = Graph()
g.parse("landlots_JPS.xml", format="xml")


# In[2]:

# print out the total number of triples in the ontology

print ("Ontology parsing begins, we find there are %s total triples in your ontology" %(len(g)))


# In[15]:

# query language that get all numerical values of the carbon emission amount of different landlots

q1 = """
     PREFIX JParkLandLots: <http://www.jparksimulator.com/JParkLandLots#>
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
            
     select distinct ?LandLot_CarbonEmissions ?ValueOf_CarbonEmissions
       where {                         
         ?LandLot_CarbonEmissions rdf:type JParkLandLots:CarbonEmissions;
         system:hasValue ?x.
         ?x system:numericalValue ?ValueOf_CarbonEmissions.}
     
     order by DESC(?ValueOf_CarbonEmissions)
"""


# In[16]:

# excute the query

x = g.query(q1)


# In[17]:

print ("There are %s total triples in your ontology that contains CO2 emission information" %(len(x)))


# In[27]:

# query language that get top 10 CO2 emitter on Jurong Island as landlot

q2 = """
     PREFIX JParkLandLots: <http://www.jparksimulator.com/JParkLandLots#>
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     PREFIX system: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>
            
     select distinct ?LandLot_CarbonEmissions ?ValueOf_CarbonEmissions
       where {                         
         ?LandLot_CarbonEmissions rdf:type JParkLandLots:CarbonEmissions;
         system:hasValue ?x.
         ?x system:numericalValue ?ValueOf_CarbonEmissions.}
     
     order by DESC(?ValueOf_CarbonEmissions)
     limit 10  
""" 


# In[29]:

# print the predicates and objectives in triples if you want

for row in g.query(q2):
    print ("Predicate:%s, objective:%s"%(row[0],row[1]))


# In[20]:

# initialize an empty vector to store the CO2 emission values

import numpy as np
A = np.zeros(len(x),dtype=np.float)


# In[21]:

# store the numerical values in the vector

i=0
for row in g.query(q1):
    a = float(row[1])
    A[i]=a
    i=i+1


# In[22]:

b= sum(A)


# In[23]:

print ("According to the current knowledge base, the total CO2 emission from Jurong Island (estimated by applying CMU model to landlots) is: %s tonnes/year" %(b))


# In[ ]:




# In[ ]:



