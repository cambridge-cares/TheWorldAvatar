##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2021         #
##########################################

"""This module declare the properties of generating UK Power Plant A-boxes OWL files"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel

class UKPowerPlant:
    
    """Default remote endpoint"""
    endpoint = EndPointConfigAndBlazegraphRepoLabel.UKPowerPlantKG
    
    """Conjunctive graph identifier"""
    identifier_powerPlantConjunctiveGraph = "http://www.theworldavatar.com/kb/ConjunctiveGraph/UKPowerPlant"
    
    """Node keys"""
    RealizationAspectKey = "PowerGenerator_"
    RequirementsAspectKey = "DesignCapacity_"
    GenerationTechnologyKey = "PlantGenerationTechnology_"
    BuiltYearKey = "YearOfBuilt_"
    OwnerKey = "Organization_"
    PowerGenerationKey = "PowerGeneration_"
    CoordinateSystemKey = "CoordinateSystem_"
    LantitudeKey = "y_coordinate_" 
    LongitudeKey = "x_coordinate_" 
    
    AdministrativeDivisionKey = "AdministrativeDivision_"
    
    valueKey = "ScalarValue_"
