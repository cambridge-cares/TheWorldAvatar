"""
# Author: qhouyee #

A module that provides all configurations, inputs, and expected results for tests.
"""
import itertools
from dataclasses import dataclass
from typing import List

# ----------------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------------
# Provide the endpoint to the Docker services
# NOTE The static IP Address is only valid for the default Docker network 
# NOTE Port must be mapped to host for this to work
KG_ENDPOINT = "http://172.17.0.1:9999/blazegraph/namespace/kb/sparql"

DEFAULT_RESPONSE = "The Ifc2Tileset agent offers the following functionality at the specified API endpoint:<BR>"
DEFAULT_RESPONSE += "<BR>"
DEFAULT_RESPONSE += "(POST) request to convert IFC models to Cesium's 3D tilesets:<BR>"
DEFAULT_RESPONSE += "&nbsp&nbsp [this_url]/api<BR>"
DEFAULT_RESPONSE += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar"

SUCCESSFUL_API_RESPONSE = "IFC model has successfully been converted. Please visit the 'data' directory for the outputs"
INVALID_PARAM_API_RESPONSE = "Missing `assetUrl` parameter in request!"

# ----------------------------------------------------------------------------------
# Test inputs and expected results
# ----------------------------------------------------------------------------------
base_namespace = "http://www.example.org/test/"
prefix = f"""\
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX base: <{base_namespace}>
PREFIX bot: <https://w3id.org/bot#>
PREFIX ontobim: <http://www.theworldavatar.com/kg/ontobim/>
PREFIX ontobuildingstructure: <http://www.theworldavatar.com/kg/ontobuildingstructure/>
PREFIX ontodevice: <https://www.theworldavatar.com/kg/ontodevice/>
PREFIX ifc2x3: <http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
"""

insert_element_query = prefix + "INSERT DATA { base:Inst_1 a bot:Element }"
select_element_query = "PREFIX bot:<https://w3id.org/bot#> SELECT ?inst WHERE {?inst a bot:Element}"
expected_select_element_result = [{'inst': base_namespace + 'Inst_1'}]


@dataclass
class Element:
    # without namespace
    iri: str
    # with namespace
    type: str
    ifc_id: str
    label: str


sample_wall = Element(
    iri="Wall_1",
    type="ontobuildingstructure:Wall",
    ifc_id="a01912518",
    label="Wall Standard"
)

sample_water_meter = Element(
    iri="Meter_1",
    type="ifc2x3:IfcBuildingElementProxy",
    ifc_id="b01351",
    label="Water Meter"
)
sample_fridge = Element(
    iri="Fridge_3",
    type="ontodevice:Fridge",
    ifc_id="c12746",
    label="Lab Fridge"
)
sample_assets = [sample_water_meter, sample_fridge]

sample_solar_panel = Element(
    iri="SolarPanel_51",
    type="ifc2x3:IfcBuildingElementProxy",
    ifc_id="d7213",
    label="Solar Panel"
)

sample_chair = Element(
    iri="Chair_4",
    type="bot:Element",
    ifc_id="k2931",
    label="Chair"
)
sample_table = Element(
    iri="Table_91",
    type="bot:Element",
    ifc_id="e9411",
    label="Table"
)
sample_furniture = [sample_chair, sample_table]


counter = itertools.count()


def make_insert_query(elements: List[Element]):
    def _element_to_triple(element: Element):
        ifc_model_rep_num = str(next(counter)).zfill(3)
        return f"""base:{element.iri} rdf:type {element.type};
                                      ontobim:hasIfcRepresentation base:IfcModelRepresentation_{ifc_model_rep_num}.
                   base:IfcModelRepresentation_{ifc_model_rep_num} rdf:type ontobim:IfcModelRepresentation;
                                                                   ontobim:hasIfcId '{element.ifc_id}'^^xsd:string;
                                                                   rdfs:label '{element.label}'^^xsd:string.
                """

    triples = "".join([_element_to_triple(e) for e in elements])
    return f"""{prefix}
        INSERT DATA {{
            {triples}
        }}
    """


insert_wall_query = make_insert_query([sample_wall])
insert_assets_query = make_insert_query(sample_assets)
insert_solar_panel_query = make_insert_query([sample_solar_panel])
insert_furniture_query = make_insert_query(sample_furniture)


sample_building_inst = "Building_1"
sample_building_iri = base_namespace + sample_building_inst
insert_building_query = f"""{prefix} 
INSERT DATA {{
    base:{sample_building_inst} bot:hasStorey base:Storey_5a9f7642-2d12-11b2-8040-cdbcaabc8e65;
                                rdf:type bot:Building;
                                ontobim:hasIfcRepresentation base:IfcBuildingRepresentation_130.
    base:IfcBuildingRepresentation_130 rdf:type ontobim:IfcModelRepresentation;
                                        ontobim:hasIfcId "0jvyVdjY901wSsMTGJsL4G";
                                        rdfs:label "TestBuilding";
}}
"""

invalid_asseturl1 = "./"
invalid_asseturl2 = "dir"
invalid_asseturl3 = "/dir/"
invalid_asseturl4 = "../../"
invalid_asseturl5 = "www.example.org"
invalid_asseturl6 = "http://www.example.com/ns/"
