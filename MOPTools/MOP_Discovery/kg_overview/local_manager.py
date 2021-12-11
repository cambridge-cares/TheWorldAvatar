'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

from kg_overview.analytics_output import kgoverview_csv
from kg_overview.analytics_operations import assemblyModelGroups
from kg_overview.analytics_operations import mopsoverview

def kgoverview():
    """Queries the KG for MOPs. Separates MOPs into Assembly Model Files. 
    Returns information on how many MOPs per Assembly Model (i.e. "uniques")
    [0] = List_AM_GBU. Returns GBUs and sets of Assembly Models where the particular
    GBU appears, i.e. [1] = List_preR2"""
    listofMOPs = mopsoverview()
    uniques = assemblyModelGroups(listofMOPs)
    kgoverview_csv(uniques[0])
    return uniques