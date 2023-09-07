##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 06 Sept 2023         #
##########################################

"""This module declare the properties of generating UK Power Plant A-boxes OWL files"""

class UKPowerPlant:
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
