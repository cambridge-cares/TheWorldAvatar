'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

from kg_overview.local_manager import kgoverview
from set_operations.lib_manager import lib_manager
from assembler.assembler import searchRadius
import os

def workflow():
    output_kgoverview = kgoverview()
    lib_manager(output_kgoverview) 
    search_analytics = searchRadius(output_kgoverview[0], output_kgoverview[2])
    return search_analytics