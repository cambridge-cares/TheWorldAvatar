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

DEFAULT_RESPONSE = "The Ifc2Tileset agent offers the following functionality at the specified API endpoint:<BR>"
DEFAULT_RESPONSE += "<BR>"
DEFAULT_RESPONSE += "(POST) request to convert IFC models to Cesium's 3D tilesets:<BR>"
DEFAULT_RESPONSE += "&nbsp&nbsp [this_url]/api<BR>"
DEFAULT_RESPONSE += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar"

# ----------------------------------------------------------------------------------
# Test inputs and expected results
# ----------------------------------------------------------------------------------
base_namespace = "http://www.example.org/test/"
prefix ="PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
prefix += "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>\n"
prefix += "PREFIX base:<"+ base_namespace +">\n"
prefix += "PREFIX bot:<https://w3id.org/bot#>\n"
prefix += "PREFIX ontobim:<http://www.theworldavatar.com/ontology/ontobim/ontoBIM#>\n"
prefix += "PREFIX ifc2x3:<http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>\n"
prefix += "PREFIX ifc4:<https://standards.buildingsmart.org/IFC/DEV/IFC4/ADD2_TC1/OWL#>\n"
prefix += "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n"

insertquery1 = prefix + "INSERT DATA{base:Inst_1 a bot:Element}"
selectquery1 = "PREFIX bot:<https://w3id.org/bot#> SELECT ?inst WHERE {?inst a bot:Element}"

expected1 = {'inst': base_namespace + 'Inst_1'}

insertquery2 = prefix + "INSERT DATA{\n"
insertquery2 += "base:Wall_1 rdf:type bot:Element;\n"
insertquery2 += "rdf:type ifc2x3:IfcWall;\n"
insertquery2 += "ontobim:hasIfcId 'a01912518'^^xsd:string;\n"
insertquery2 += "rdfs:label 'Wall Standard'^^xsd:string.\n"
insertquery2 += "}"

expected_assets1 = ["building"]

meterid = "b01351"
boxid = "c12746"
panelid = "d7213"

insertquery3 = prefix + "INSERT DATA{\n"
insertquery3 += "base:Meter_1 rdf:type bot:Element;\n"
insertquery3 += "   rdf:type ifc2x3:IfcBuildingElementProxy;\n"
insertquery3 += "   ontobim:hasIfcId '"+ meterid + "'^^xsd:string;\n"
insertquery3 += "   rdfs:label 'Water Meter'^^xsd:string.\n"
insertquery3 += "base:ElectricWireBox_3 rdf:type bot:Element;\n"
insertquery3 += "   rdf:type ifc2x3:IfcFurnishingElement;\n"
insertquery3 += "   ontobim:hasIfcId '"+ boxid + "'^^xsd:string;\n"
insertquery3 += "   rdfs:label 'Electric Wire Box'^^xsd:string.\n"
insertquery3 += "base:SolarPanel_51 rdf:type bot:Element;\n"
insertquery3 += "   rdf:type ifc2x3:IfcBuildingElementProxy;\n"
insertquery3 += "   ontobim:hasIfcId '"+ panelid + "'^^xsd:string;\n"
insertquery3 += "   rdfs:label 'Solar Panel'^^xsd:string.\n"
insertquery3 += "}"

name_col = "name"
file_col = "file"
expected_assets2 = ["building", "asset1", "furniture", "solarpanel"]