##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Date: 16 April 2021                    #
##########################################

"""This class defines the T-boxes used by UK digital twin"""

class UKDigitalTwinTBox:
    """DBPedia"""
    dbr = "http://dbpedia.org/resource/"
    dbp = "https://dbpedia.org/property/"
    dbo = "https://dbpedia.org/ontology/"
    
    """DBPedia property"""
    
    """DBPedia class"""
    UK = dbr + "United_Kingdom"
    
    """OntoCAPE"""
    ontocape_upper_level_system = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"    
    ontocape_derived_SI_units =  "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#"
    ontoecape_space_and_time_extended = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"
    ontoecape_space_and_time = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#"
    ontoecape_technical_system = "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#"
    ontocape_network_system = "http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#"
    ontocape_mathematical_model = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#"
    ontocape_physical_dimension = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#"
    ontocape_coordinate_system = "http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#"
    ontocape_SI_units = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#"
    ontocape_mathematical_relation = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#"
    ontocape_geometry = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#"
    
    meta_model_topology = "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#"
       
    """OntoEIP"""
    ontoeip_system_realization = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#"
    ontoeip_system_function = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#"
    ontoeip_powerplant = "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#"
    ontoeip_upper_level_system_v1 = "http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#"
    ontoeip_system_performance = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#"
    ontoeip_system_requirement = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_requirement.owl#" # Undefined 


    """OntoPowSys"""
    ontopowsys_PowSysRealization = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#"
    ontopowsys_PowSysFunction = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#"
    ontopowsys_PowerSystemModel = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#"
    ontopowsys_PowSysPerformance = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#"
