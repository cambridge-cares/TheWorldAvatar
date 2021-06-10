##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 10 June 2021         #
##########################################

""" This module defines the Graph Store used in specifying the local storage of the graph"""
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.plugins.memory import IOMemory
            
def LocalGraphStore(storeType = 'default'):   
    if storeType == 'default':    
        graphstore = IOMemory()
    elif storeType =='sleepycat':
        graphstore = Sleepycat()
        graphstore.__open = True
        graphstore.context_aware = True
    return graphstore  
