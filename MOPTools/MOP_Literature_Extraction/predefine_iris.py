# -----------------------------------------------
# Base iri for the ontologies
# -----------------------------------------------
ONTOSYN_BASE        = "https://www.theworldavatar.com/kg/OntoSyn/"
SKOS_BASE           = "http://www.w3.org/2004/02/skos/core#"
ONTOMEASURE_BASE    = "http://www.ontology-of-units-of-measure.org/resource/om-2/"
RDFS_BASE           = "http://www.w3.org/2000/01/rdf-schema#"
BIBO_BASE           = "http://purl.org/ontology/bibo/"
SPECIES_BASE        = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"
MOPS_BASE           = "https://www.theworldavatar.com/kg/ontomops/"
XSD_BASE            = "http://www.w3.org/2001/XMLSchema#"
RDF_BASE            = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
ONTOCAPE_BASE       = "http://www.theworldavatar.com/ontology/ontocape/"
DAML_BASE           = "http://www.daml.org/2003/01/periodictable/PeriodicTable#"
SPECIES_NEW_BASE    = "http://www.theworldavatar.com/kb/ontospecies/"
# -----------------------------------------------
# Characteristic Peaks (NMR & IR Spectroscopy)
# -----------------------------------------------
IR_NA               = f"{SPECIES_BASE}CharacteristicPeak_f6cce625-9d69-4491-bd9d-b096114db7af"
NMR_NA              = f"{SPECIES_BASE}CharacteristicPeak_920795e4-f412-4ad6-807b-8da69519a332"
# -----------------------------------------------
# Chemical Species
# -----------------------------------------------
KBR                 = f"{SPECIES_NEW_BASE}Species_3b6489f0-a36c-4734-962b-521c8fab639b"
# -----------------------------------------------
# Chemical Elements
# -----------------------------------------------
HYDROGEN            = f"{DAML_BASE}Element_43cfac3b-81db-4338-bfb1-b0b3386f7473"
CARBON              = f"{DAML_BASE}Element_f1a5025b-7e20-4a6a-821e-a7b6c0473b8c"
OXYGEN              = f"{DAML_BASE}Element_6a6be1ce-2021-4634-aed4-6a77488765df"
NITROGEN            = f"{DAML_BASE}Element_a9a7806c-f077-4eb2-b5b0-099d51033b7b"
UNKNOWN_ELEMENT     = f"{DAML_BASE}Element_e253b6ca-c169-4e60-b6be-46b95e045a85"
# -----------------------------------------------
# Vessel Types
# -----------------------------------------------
VESSEL_SS_TEFLON    = f"{ONTOSYN_BASE}VesselType_eb0f5942-d36b-47b1-86f0-725c1549fa2e"
GLASS_VIAL          = f"{ONTOSYN_BASE}VesselType_90589d23-44e8-4698-acdf-bee3e44df96f"
QUARTZ_TUBE         = f"{ONTOSYN_BASE}VesselType_06304c23-7926-45d2-841d-690b5de16ed0"
ROUND_BOTTOM_FLASK  = f"{ONTOSYN_BASE}VesselType_5a7d7ec9-44d5-4280-8467-f9f624374a9d"
GLASS_SCINTILATION_VIAL     = f"{ONTOSYN_BASE}VesselType_b67ea47b-7849-4aac-b0fd-e2715a4ac034"
PYREX_TUBE          = f"{ONTOSYN_BASE}VesselType_080ad74b-950d-4651-a87c-5aa96d5ffb52"
SCHLENK             = f"{ONTOSYN_BASE}VesselType_080zd54b-230c-4341-e87g-5ta46d2fgh91"
UNDEFINED_VESSEL    = f"{ONTOSYN_BASE}VesselType_183ad74b-950d-4631-a47c-5aa91d5ffb12"
# -----------------------------------------------
# Units of Measurement
# -----------------------------------------------
DEGREE_CELSIUS      = f"{ONTOMEASURE_BASE}degreeCelsius"
KELVIN              = f"{ONTOMEASURE_BASE}kelvin"
DEGREE_CELSIUS_HOUR = f"{ONTOMEASURE_BASE}degreeCelsiusPerHour"
DEGREE_CELSIUS_MIN  = f"{ONTOMEASURE_BASE}degreeCelsiusPerMinute-Time"
DURATION_H          = f"{ONTOMEASURE_BASE}hour"
DURATION_DAY        = f"{ONTOMEASURE_BASE}day"
DURATION_S          = f"{ONTOMEASURE_BASE}second-Time"
DURATION_WEEK       = f"{ONTOMEASURE_BASE}week"
DURATION_MIN        = f"{ONTOMEASURE_BASE}minute-Time"
TEMPERATURE_RATE_DEGS   = f"{ONTOMEASURE_BASE}degreeCelsiusPerSecond-Time"
MOLE_PER_LITRE      = f"{ONTOMEASURE_BASE}molePerLitre"
GRAMS               = f"{ONTOMEASURE_BASE}gram"
MILI_GRAMS          = f"{ONTOMEASURE_BASE}milligram"
MOLE                = f"{ONTOMEASURE_BASE}mole"
MMOLE               = f"{ONTOMEASURE_BASE}millimole"
MLITRE              = f"{ONTOMEASURE_BASE}millilitre"
REVOLUTIONS_PER_MINUTE  = f"{ONTOMEASURE_BASE}revolutionPerMinute-Time"
DROP                = f"{ONTOMEASURE_BASE}drop"
UNKNOWN_UNIT        = f"{ONTOMEASURE_BASE}unknown"
# -----------------------------------------------
# Vessel Environments
# -----------------------------------------------
N2_ATMO             = f"{ONTOSYN_BASE}VesselEnvironment_434aa6e1-3ac6-4a08-a208-fbc23e78a758"
AR_ATMO             = f"{ONTOSYN_BASE}VesselEnvironment_65b5af5d-349d-467c-bd14-b239d4e94376"
AIR_ATMO            = f"{ONTOSYN_BASE}VesselEnvironment_bd2ef29a-1c5c-40eb-a9b2-84f1a3fda734"
UNKNOWN_ATMO        = f"{ONTOSYN_BASE}VesselEnvironment_1f70dc2c-5a37-491a-89ec-0897f9dcb7b8"
# -----------------------------------------------
# General Unknowns
# -----------------------------------------------
YIELD_VALUE         = f"{ONTOSYN_BASE}YieldValue_3ed5e18b-5206-405d-ada0-382071f73f74"
YIELD_INSTANCE      = f"{ONTOSYN_BASE}Yield_3ed5e18b-5206-405d-ada0-382071f73f74"
CHEMICAL_INPUT      = f"{ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74"
UNKNWON_MOP         = f"{MOPS_BASE}MetalOrganicPolyhedra_1d981ba2-4072-47ef-9ecd-b9f5cc06a50a"
# -----------------------------------------------
# Separation Techniques
# -----------------------------------------------
CENTRIFUGE          = f"{ONTOSYN_BASE}SeparationType_5aa57330-613e-437d-a22b-dc10833a50b8"
COLUMN              = f"{ONTOSYN_BASE}SeparationType_aea2a49f-067f-4818-8abc-544dd8696ba8"
WASH                = f"{ONTOSYN_BASE}SeparationType_61233c76-a0e5-4cb0-8c5c-ab8347955ea6"
EXTRACTION          = f"{ONTOSYN_BASE}SeparationType_c6d7ff74-4bdb-47b8-bcd8-fc40d9fbfb87"
UNKNOWN_SEP         = f"{ONTOSYN_BASE}SeparationType_9ff2a8f7-3c9c-4419-9f3a-76b34d8629c0"
UNKNOWN_STEP        = f"{ONTOSYN_BASE}SynthesisStep_ddb7ceda-13d2-461a-a63e-9e7df3116882"
PERCENTAGE          = f"{ONTOMEASURE_BASE}percent"


