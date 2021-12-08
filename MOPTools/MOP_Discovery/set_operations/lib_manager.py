'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

from set_operations.lib1_creation import lib1_Creation
from set_operations.cbuoverlap import cbuoverlap
from set_operations.lib2_creation import lib2_creation
from set_operations.lib2_creation import add_unchanged_files

def lib_manager(output_kgoverview):
    """The Lib Manager prepares cbu libraries associated with gbus of particular assembly model."""
    lib1_Creation(output_kgoverview[0]) 
    cbuoverlap(output_kgoverview[1])
    lib2_creation(output_kgoverview[1])
    add_unchanged_files(output_kgoverview[0])