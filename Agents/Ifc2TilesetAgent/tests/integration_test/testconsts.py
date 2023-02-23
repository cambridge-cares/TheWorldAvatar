"""
# Author: qhouyee #

A module that provides all configurations, inputs, and expected results for tests.
"""
# ----------------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------------
# Provide the endpoint to the Docker services
# NOTE The static IP Address is only valid for the default Docker network 
# NOTE Port must be mapped to host for this to work
KG_ENDPOINT = "http://172.17.0.1:9999/blazegraph/namespace/kb/sparql"

ROOT_TILE = dict(
    x_center=40,
    y_center=0,
    z_center=15,
    length=100,
    width=100,
    height=10
)
CHILD_TILE = dict(
    x_center=10,
    y_center=0,
    z_center=5,
    length=50,
    width=50,
    height=6
)

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
prefix = f"""PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
    PREFIX base:<{base_namespace}>
    PREFIX bot:<https://w3id.org/bot#>
    PREFIX ontobim:<http://www.theworldavatar.com/ontology/ontobim/ontoBIM#>
    PREFIX ifc2x3:<http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>
    PREFIX ifc4:<https://standards.buildingsmart.org/IFC/DEV/IFC4/ADD2_TC1/OWL#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n"""

insert_element_query = prefix + "INSERT DATA { base:Inst_1 a bot:Element }"
select_element_query = "PREFIX bot:<https://w3id.org/bot#> SELECT ?inst WHERE {?inst a bot:Element}"
expected_select_element_result = [{'inst': base_namespace + 'Inst_1'}]

insert_wall_query = prefix + """
    INSERT DATA {
        base:Wall_1 rdf:type bot:Element;
                    rdf:type ifc2x3:IfcWall;
                    ontobim:hasIfcId 'a01912518'^^xsd:string;
                    rdfs:label 'Wall Standard'^^xsd:string. 
    }"""

expected_assets1 = ["building"]

sample_meter_id = "b01351"
sample_box_id = "c12746"
sample_panel_id = "d7213"

insert_assets_query = f"""{prefix}
    INSERT DATA {{
        base:Meter_1 rdf:type bot:Element;
                     rdf:type ifc2x3:IfcBuildingElementProxy;
                     ontobim:hasIfcId '{sample_meter_id}'^^xsd:string;
                     rdfs:label 'Water Meter'^^xsd:string.
        base:ElectricWireBox_3 rdf:type bot:Element;
                               rdf:type ifc2x3:IfcFurnishingElement;
                               ontobim:hasIfcId '{sample_box_id}'^^xsd:string;
                               rdfs:label 'Electric Wire Box'^^xsd:string.
        base:SolarPanel_51 rdf:type bot:Element;
                           rdf:type ifc2x3:IfcBuildingElementProxy;
                           ontobim:hasIfcId '{sample_panel_id}'^^xsd:string;
                           rdfs:label 'Solar Panel'^^xsd:string.
    }}"""

sample_building_inst = "Building_1"
sample_building_iri = base_namespace + sample_building_inst
insert_building_query = f"""{prefix} 
    INSERT DATA {{
        base:{sample_building_inst} bot:hasStorey base:Storey_5a9f7642-2d12-11b2-8040-cdbcaabc8e65;
        rdf:type bot:Building;
        ontobim:hasIfcRepresentation base:IfcBuildingRepresentation_130.
    }}"""

expected_assets2 = ["building", "asset1", "furniture", "solarpanel"]

invalid_asseturl1 = "./"
invalid_asseturl2 = "dir"
invalid_asseturl3 = "/dir/"
invalid_asseturl4 = "../../"
invalid_asseturl5 = "www.example.org"
invalid_asseturl6 = "http://www.example.com/ns/"
