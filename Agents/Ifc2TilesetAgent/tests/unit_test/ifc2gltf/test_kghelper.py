"""
# Author: qhouyee #

A test suite for the agent.ifc2gltf.kghelper submodule.
"""

# Third party import
import pandas as pd

# Self import
from agent.ifc2gltf.kghelper import create_metadata_query, classify_file_name


def test_create_query():
    """
    Tests create_query()
    """
    expected = "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
    expected += "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>\n"
    expected += "PREFIX bot:<https://w3id.org/bot#>\n"
    expected += "PREFIX ontobim:<http://www.theworldavatar.com/ontology/ontobim/ontoBIM#>\n"
    expected += "PREFIX ifc2x3:<http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>\n"
    expected += "PREFIX ifc4:<https://standards.buildingsmart.org/IFC/DEV/IFC4/ADD2_TC1/OWL#>\n"
    expected += "SELECT ?iri ?uid ?name WHERE {?iri rdf:type bot:Element.\n"
    expected += "?iri ontobim:hasIfcId ?uid.\n"
    expected += "?iri rdfs:label ?name.\n"
    expected += "?iri rdf:type ?ifcclass.\n"
    expected += "VALUES ?ifcclass {ifc2x3:IfcBuildingElementProxy ifc2x3:IfcFurnishingElement "
    expected += "ifc2x3:IfcFlowTerminal ifc4:IfcBuildingElementProxy ifc4:IfcFurnishingElement "
    expected += "ifc4:IfcFlowTerminal } \n}"
    assert expected == create_metadata_query()


def test_classify_file_name():
    """
    Tests classify_file_name()
    """
    # Generate sample data
    data = {
            "name": ["Water Meter", "Solar Panel", "Desk", "Chemistry Robot (Detectors)"]
            }
    sampledf = pd.DataFrame(data)
    # Execute method
    sampledf = classify_file_name(sampledf)
    # Test assertions
    assert sampledf['file'].iloc[0] == "asset1"
    assert sampledf['file'].iloc[1] == "solarpanel"
    assert sampledf['file'].iloc[2] == "furniture"
    assert sampledf['file'].iloc[3] == "asset2"
