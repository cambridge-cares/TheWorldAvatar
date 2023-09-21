RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
OWL = "http://www.w3.org/2002/07/owl#"
RDFS = "http://www.w3.org/2000/01/rdf-schema#"

RDF_TYPE =  RDF + 'type'
OWL_SAMEAS = OWL + "sameAs"





"""OntoCAPE"""
ONTOCAPE_UPPER_LEVEL_SYSTEM = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#"    
ONTOCAPE_DERIVED_SI_UNITS =  "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#"
ONTOECAPE_SPACE_AND_TIME_EXTENDED = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#"
ONTOECAPE_SPACE_AND_TIME = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#"
ONTOECAPE_TECHNICAL_SYSTEM = "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#"
ONTOCAPE_NETWORK_SYSTEM = "http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#"
ONTOCAPE_MATHEMATICAL_MODEL = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#"
ONTOCAPE_PHYSICAL_DIMENSION = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#"
ONTOCAPE_COORDINATE_SYSTEM = "http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#"
ONTOCAPE_SI_UNITS = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#"
ONTOCAPE_MATHEMATICAL_RELATION = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#"
ONTOCAPE_GEOMETRY = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#"

META_MEDOL_TOPOLOGY = "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#"

ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM = ONTOCAPE_UPPER_LEVEL_SYSTEM + "isComposedOfSubsystem"
META_MEDOL_TOPOLOGY_HASOUTPUT = META_MEDOL_TOPOLOGY + "hasOutput"
ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT = ONTOECAPE_TECHNICAL_SYSTEM + "hasRealizationAspect"
ONTOECAPE_TECHNICAL_SYSTEM_HASREQUIREMENTSASPECT = ONTOECAPE_TECHNICAL_SYSTEM + "hasRequirementsAspect"

ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE = ONTOCAPE_UPPER_LEVEL_SYSTEM + "hasValue"
ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE = ONTOCAPE_UPPER_LEVEL_SYSTEM + "numericalValue"

ONTOCAPE_UPPER_LEVEL_SYSTEM_CONTAINS = ONTOCAPE_UPPER_LEVEL_SYSTEM + "contains"

ONTOCAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT = ONTOECAPE_TECHNICAL_SYSTEM + "hasRealizationAspect"

    
"""OntoEIP"""
ONTOEIP_SYSTEM_REALIZATION = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#"
ONTOEIP_SYSTEM_FUNCTION = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#"
ONTOEIP_POWERPLANT = "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#"
ONTOEIP_UPPER_LEVEL_SYSTEM_V1 = "http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#"
ONTOEIP_SYSTEM_PERFORMANCE = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#"
ONTOEIP_SYSTEM_REQUIREMENT = "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_requirement.owl#" # Undefined 
ONTOPOWSYS = "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#"

ONTOEIP_SYSTEM_REQUIREMENT_DESIGNCAPACITY = ONTOEIP_SYSTEM_REQUIREMENT + "DesignCapacity"

"""OntoPowSys"""
ONTOPOWSYS_POWSYSREALIZATION = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#"
ONTOPOWSYS_POWSYSFUNCTION = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#"
ONTOPOWSYS_POWERSYSTEMMODEL = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#"
ONTOPOWSYS_POWSYSPERFORMANCE = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#"
ONTOPOWSYS_ONTOPOWSYS = "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#"

ONTOPOWSYS_POWSYSREALIZATION_BUSNODE = ONTOPOWSYS_POWSYSREALIZATION + "BusNode"

"""OntoSDG"""
ONTOSDG = "http://theworldavatar.com/ontology/ontosdg/OntoSDG.owl#"

"""OntoSpecies"""
ONTOSPECIES = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"

"""Bibtex"""
BIBTEX = "https://zeitkunst.org/bibtex/0.2/bibtex.owl#"

"""OntoEnergySystem"""
ONTOENERGYSYSTEM = "http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#"

ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE = ONTOENERGYSYSTEM + "hasWGS84LatitudeLongitude"

ONTOENERGYSYSTEM_HASRELEVANTPLACE = ONTOENERGYSYSTEM + "hasRelevantPlace"


"""GB"""
GB = "https://dbpedia.org/page/Great_Britain"