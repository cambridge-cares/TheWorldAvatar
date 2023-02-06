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

# ----------------------------------------------------------------------------------
# Test inputs and expected results
# ----------------------------------------------------------------------------------
insertquery1 = "PREFIX base:<http://www.example.org/test/>\
    PREFIX bot:<http://www.theworldavatar.com/ontology/ontobim/ontoBIM#>\
    INSERT DATA{base:Inst_1 a bot:Element}"
selectquery1 = "PREFIX bot:<http://www.theworldavatar.com/ontology/ontobim/ontoBIM#>\
    SELECT ?inst WHERE {?inst a bot:Element}"
[{'inst': 'http://www.example.org/test/Inst_1'}]

expected1 = {'inst': 'http://www.example.org/test/Inst_1'}