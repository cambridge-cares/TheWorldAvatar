"""
# Author: qhouyee, picas9dan #

A module that provides all configurations, inputs, and expected results for tests.
"""

# Standard library imports
import itertools
from dataclasses import dataclass

# ----------------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------------
# Provide the endpoint to the Docker services
# NOTE The static IP Address is only valid for the default Docker network
# NOTE Port must be mapped to host for this to work
KG_ENDPOINT = "http://172.17.0.1:9999/blazegraph/namespace/kb/sparql"

SUCCESSFUL_API_RESPONSE = "IFC model has successfully been converted. Please visit the 'data' directory for the outputs"

SAMPLE_YAML_PATH = "./config/properties.yaml"

# ----------------------------------------------------------------------------------
# Test inputs and expected results
# ----------------------------------------------------------------------------------
base_namespace = "http://www.example.org/test/"
prefix = f"""\
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX base: <{base_namespace}>
PREFIX bot: <https://w3id.org/bot#>
PREFIX ontobim: <https://www.theworldavatar.com/kg/ontobim/>
PREFIX ontobuildingstructure: <https://www.theworldavatar.com/kg/ontobuildingstructure/>
PREFIX ontodevice: <https://www.theworldavatar.com/kg/ontodevice/>
PREFIX ifc2x3: <http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
"""

insert_element_query = prefix + "INSERT DATA { base:Inst_1 a bot:Element }"
select_element_query = "PREFIX bot:<https://w3id.org/bot#> SELECT ?inst WHERE {?inst a bot:Element}"
expected_select_element_result = [{"inst": base_namespace + "Inst_1"}]


@dataclass
class OntoBimTestElement:
    # without namespace
    iri: str
    # with namespace
    type: str
    ifc_id: str
    label: str


sample_wall = OntoBimTestElement(
    iri="Wall_1",
    type="ontobuildingstructure:Wall",
    ifc_id="a01912518",
    label="Wall Standard"
)

# OTHER ASSETS
sample_water_meter = OntoBimTestElement(
    iri="Meter_1",
    type="ifc2x3:IfcBuildingElementProxy",
    ifc_id="b01351",
    label="Water Meter"
)
sample_fridge = OntoBimTestElement(
    iri="Fridge_3",
    type="ontodevice:Fridge",
    ifc_id="c12746",
    label="Lab Fridge"
)

# SOLAR PANEL
sample_solar_panel = OntoBimTestElement(
    iri="SolarPanel_51",
    type="ifc2x3:IfcBuildingElementProxy",
    ifc_id="d7213",
    label="Solar Panel"
)

# SEWAGE NETWORK
sample_sewage_manhole = OntoBimTestElement(
    iri="Manhole_193",
    type="ifc2x3:IfcBuildingElementProxy",
    ifc_id="76sa7",
    label="Manhole"
)

# FURNITURE
sample_chair = OntoBimTestElement(
    iri="Chair_4",
    type="bot:Element",
    ifc_id="k2931",
    label="Chair"
)
sample_table = OntoBimTestElement(
    iri="Table_91",
    type="bot:Element",
    ifc_id="e9411",
    label="Table"
)

SAMPLE_ONTOBIM_ELEMENT_STORE = dict(
    water_meter=sample_water_meter,
    fridge=sample_fridge,
    solar_panel=sample_solar_panel,
    sewage_network=sample_sewage_manhole,
    chair=sample_chair,
    table=sample_table
)

counter = itertools.count()


def _elem_to_triple(e: OntoBimTestElement):
    ifc_model_rep_num = str(next(counter)).zfill(3)
    return f"""\
base:{e.iri} rdf:type {e.type};
             ontobim:hasIfcRepresentation base:IfcModelRepresentation_{ifc_model_rep_num}.
base:IfcModelRepresentation_{ifc_model_rep_num} rdf:type ontobim:IfcModelRepresentation;
                                                ontobim:hasIfcId '{e.ifc_id}'^^xsd:string;
                                                rdfs:label '{e.label}'^^xsd:string.
"""


CONTENT_METADATA_SCHEMA = {"classes":
                           {"ContentMetaData":
                            {"description": "A metadata class for all content including building and individual assets",
                             "name": "Content metadata",
                             "properties": {
                                 "name": {
                                     "description": "Name of the asset/building",
                                     "type": "STRING"
                                 },
                                 "iri": {
                                     "description": "Data IRI of the asset/building",
                                     "type": "STRING"
                                 }
                             }}
                            }
                           }

SAMPLE_BUILDING_INST = "Building_1"
SAMPLE_BUILDING_IRI = base_namespace + SAMPLE_BUILDING_INST
SAMPLE_BUILDING_NAME = "TestBuilding"
building_nameless_triple = f"""\
base:{SAMPLE_BUILDING_INST} bot:hasStorey base:Storey_5a9f7642-2d12-11b2-8040-cdbcaabc8e65;
                            rdf:type bot:Building;
                            ontobim:hasIfcRepresentation base:IfcBuildingRepresentation_130.
"""

building_triple = building_nameless_triple + f"""\
base:IfcBuildingRepresentation_130 rdf:type ontobim:IfcModelRepresentation;
                                   ontobim:hasIfcId '0jvyVdjY901wSsMTGJsL4G'^^xsd:string;
                                   rdfs:label '{SAMPLE_BUILDING_NAME}'^^xsd:string.
"""

SAMPLE_BUILDING_METADATA = {
    "class": "ContentMetaData",
    "properties": {
        "name": SAMPLE_BUILDING_NAME,
        "iri": SAMPLE_BUILDING_IRI
    }
}

SAMPLE_SOLAR_IRI = base_namespace + "Panel_123"
SAMPLE_SOLAR_NAME = "Solar panels"
SAMPLE_SOLAR_METADATA = {
    "class": "ContentMetaData",
    "properties": {
        "name": SAMPLE_SOLAR_NAME,
        "iri": SAMPLE_SOLAR_IRI
    }
}

SAMPLE_SEWAGE_IRI = base_namespace + "Sewage_123"
SAMPLE_SEWAGE_NAME = "Sewage city network"
SAMPLE_SEWAGE_METADATA = {
    "class": "ContentMetaData",
    "properties": {
        "name": SAMPLE_SEWAGE_NAME,
        "iri": SAMPLE_SEWAGE_IRI
    }
}

SAMPLE_ROOT_IRI = base_namespace + "Root_12"
SAMPLE_ROOT_NAME = "Root Content"

SAMPLE_ROOT_METADATA = {
    "class": "ContentMetaData",
    "properties": {
        "name": SAMPLE_ROOT_NAME,
        "iri": SAMPLE_ROOT_IRI
    }
}

SAMPLE_ONTOBIM_TRIPLESTORE = dict(
    building=building_triple,
    nameless_building = building_nameless_triple,
    wall=_elem_to_triple(sample_wall),
    water_meter=_elem_to_triple(sample_water_meter),
    fridge=_elem_to_triple(sample_fridge),
    solar_panel=_elem_to_triple(sample_solar_panel),
    sewage_network=_elem_to_triple(sample_sewage_manhole),
    chair=_elem_to_triple(sample_chair),
    table=_elem_to_triple(sample_table)
)

SAMPLE_ONTOBIM_GEOM_STORE = dict(
    # (1 x 1 x 1) box on ground floor
    water_meter=((0., 2., 0.), (1., 3., 1.)),
    # (1 x 1 x 2) box on second floor
    fridge=((4., 0., 3.), (5., 1., 5.)),
    # (3 x 3 x 0.5) box on third floor
    solar_panel=((0., 0., 6.), (3., 3., 6.5)),
    # (2 x 2 x 2) box on ground floor
    sewage_network=((1., 1., 0.), (3., 3.,2.)),
    # (0.5, 0.5, 1) box on ground floor
    chair=((3., 0., 0.), (3.5, 0.5, 1.)),
    # (1 x 1 x 0.5) box on second floor
    table=((0., 1., 3.), (1., 2., 3.5))
)
