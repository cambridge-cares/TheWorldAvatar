##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2021         #
##########################################

"""This module declare the properties of generating UK Power Plant A-boxes OWL files"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLable

class UKPowerPlant:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\UK_Power_Plant_KG\\"
    
    """Default path of SleepycatStoragePath"""
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"
    
    """Default remote endpoint"""
    endpoint = EndPointConfigAndBlazegraphRepoLable.UKPowerPlantKG
    
    """Conjunctive graph identifier"""
    identifier_powerPlantConjunctiveGraph = "http://www.theworldavatar.com/kb/ConjunctiveGraph/UKPowerPlant"
    
    """Node keys"""
    RealizationAspectKey = "PowerGenerator_"
    RequirementsAspectKey = "DesignCapacity_"
    BuiltYearKey = "YearOfBuilt_"
    OwnerKey = "Owner_"
    CoordinateSystemKey = "CoordinateSystem_"
    LantitudeKey = "y_coordinate_" 
    LongitudeKey = "x_coordinate_" 
    
    valueKey = "value_"
