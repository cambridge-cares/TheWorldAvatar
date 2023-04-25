##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 12 July 2022         #
##########################################

"""This module declare the properties of generating UK population density A-boxes OWL files"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel

class UKPopulationDensity:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "/mnt/c/Users/wx243/Desktop/KGB/1 My project/1 Ongoing/4 UK Digital Twin/A_Box/UK_Population_Density/"
    
    """Default remote endpoint"""
    endpoint = EndPointConfigAndBlazegraphRepoLabel.UKPowerPlantKG
    
    """Node keys"""
    LocationKey = "Location_"
    PopulationKey = "Population_"    
    valueKey = "ScalarValue_"
