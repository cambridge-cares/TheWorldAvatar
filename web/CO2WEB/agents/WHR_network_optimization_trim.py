
# coding: utf-8

# In[26]:

from rdflib import Graph

from rdflib import URIRef, BNode, Literal

import numpy as np

import scipy as sp

from scipy.optimize import linprog

import networkx as nx

import matplotlib.pyplot as plt
import json

import os
dir = os.path.dirname(__file__)
import sys

for arg in sys.argv:
    print (arg)

print (sys.argv[1])

# In[27]:

#g = Graph()
#g.parse(sys.argv[1], format="xml")
#C:/irp3-WebJPS-git/CO2WEB/testFiles/wasteheatnetwork.owl
ResultJSON = {}
def add2Results (name, value):
    ResultJSON[name] = value

# In[28]:

#print ("Ontology parsing begins, we find there are %s total tuples in your ontology" %(len(g)))
#add2Results("tupleNumber", len(g))

# In[29]:

#input("Press Enter to continue...")


# In[98]:

A = np.zeros((5,2))
B = np.zeros((5,2))


#SourcePlantQualities
A[0,0] = float(sys.argv[1])
A[1,0] = float(sys.argv[2])
A[2,0] = float(sys.argv[3])
A[3,0] = float(sys.argv[4])
A[4,0] = float(sys.argv[5])

#SinkPlantQualities
A[0,1] = float(sys.argv[6])
A[1,1] = float(sys.argv[7])
A[2,1] = float(sys.argv[8])
A[3,1] = float(sys.argv[9])
A[4,1] = float(sys.argv[10])



B[0,0] = float(sys.argv[11])
B[1,0] = float(sys.argv[12])
B[2,0] = float(sys.argv[13])
B[3,0] = float(sys.argv[14])
B[4,0] = float(sys.argv[15])

B[0,1] = float(sys.argv[16])
B[1,1] = float(sys.argv[17])
B[2,1] = float(sys.argv[18])
B[3,1] = float(sys.argv[19])
B[4,1] = float(sys.argv[20])


SourcePlantQuantities = [B[0,0],B[1,0],B[2,0],B[3,0],B[4,0]]

SinkPlantQuantities = [B[0,1],B[1,1],B[2,1],B[3,1],B[4,1]]
SourcePlantQualities = [A[0,0],A[1,0],A[2,0],A[3,0],A[4,0]]

SinkPlantQualities = [A[0,1],A[1,1],A[2,1],A[3,1],A[4,1]]


#print ("Ontology parsing has successfully finished.")


# In[102]:

#input("Press Enter to continue...")


# In[ ]:




# In[103]:

st1 = 'sc'
st2 = 'sk'


# In[104]:

def optimization (x, y):
    SourceT = x[:,0]
    SinkT = x[:,1]
    SourceH = y[:,0]
    SinkH = y[:,1]
    count = 0
    for i in range(SourceT.shape[0]):
           for j in range(SourceT.shape[0]):  
              if (j!=i): 
                    #print ("source temperature is " +  str(SinkT[i,0]) + " sink temperature is " + str(SinkT[j,0]) )
                    criteria = SourceT[i] - SinkT[j]
                    if criteria >= 0:
                        if count == 0: M = np.matrix([SourceH[i], SinkH[j], i+1, j+1])
                        else: 
                            newrow = np.matrix([SourceH[i], SinkH[j], i+1, j+1])
                            M = np.vstack([M, newrow])
                        #print ("waste heat recovery from " + str(i+1) +  " to " + str(j+1) + " is possible")
                        #print (' ')
                        count = count + 1
                    #else:
                        #print ("waste heat recovery from " + str(i+1) +  " to " + str(j+1) + " is impossible")
                        #print (' ')
    #print (M)      
    #print (count)
    #print ('After transportation network modelling, we find that:\n There are ' + str(count) + 
    #   ' total possible waste heat recovery energy flows in the network')
    #print ('')
    #print ('There possible energy flows are:\n')

    add2Results("flowNumber", str(count))
    flowList = []
    for i in range(count):
       #print ("From plant %s to plant %s" %(M[i,2],M[i,3]))
       flowList.append([M[i,2],M[i,3]])
    add2Results("flowList", flowList)


    
    # optimization of energy network, formulation and solution of the otpimization problem (step 3 in the paper)
    c = [-1, -1, -1, -1, -1, -1, -1]
    AA = [[1,0,0,0,0,0,0],[0,1,0,0,0,0,0],[0,0,1,1,1,1,0],[0,0,0,0,0,0,1],[1,1,0,1,0,0,1],[0,0,1,0,0,0,0],[0,0,0,0,1,0,0],[0,0,0,0,0,1,0]]
    bb = [5766,2164,2150,1467,895,1512,1645,994]
    x0_bounds = (0, None)
    x1_bounds = (0, None)
    x2_bounds = (0, None)
    x3_bounds = (0, None)
    x4_bounds = (0, None)
    x5_bounds = (0, None)
    x6_bounds = (0, None)
    
    res = linprog(c, A_ub=AA, b_ub=bb, bounds=(x0_bounds, x1_bounds,x2_bounds, x3_bounds, x4_bounds,x5_bounds, x6_bounds),
              options={"disp": True})
    #print (res)
    
    #print ('')
    #print ('After network optimization, we find that the optimal waste heat recovery network is:\n')
    modifyList = []
    for i in range(count):
      if i < len(res.x) and res.x[i] != 0:
         modifyList.append([M[i,2],M[i,3],res.x[i]])
         #print ("%s kW waste heat from plant %s to plant %s" %(res.x[i],M[i,2],M[i,3])) \
    add2Results("modifyList", modifyList)

    #print ('')
    #print ('Under such a design configuration, the whole eco-industrial park can save %s kW energy' %(-res.fun))
    add2Results("saveNumber", -res.fun)
    print ("JSON"+str(json.dumps(ResultJSON)))







# In[109]:

A1=A
B1=B

ran1 = np.random.rand(5,2)

ran2 = np.random.rand(5,2)

A1 = A + A * ran1 * 0.5

B1 = B + B * ran2 * 0.5


# In[110]:

optimization(A1, B1)


# In[ ]:




# In[ ]:




# In[ ]:



