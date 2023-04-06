import pyderivationagent.data_model.iris as iris
from rdflib import XSD
import pytest

# test_triples_for_check_exist = [
#     ['http://s', 'http://p0', 'http://o', None], # object property
#     ['http://s', 'http://p1', 'http://o', XSD.string], # data property
#     ['http://s', 'http://p2', 'o', XSD.string.toPython()], # data property with string
#     ['http://s', 'http://p3', 3, XSD.int.toPython()], # data property with int
#     ['http://s', 'http://p4', 3.14, XSD.double.toPython()], # data property with float
#     ['http://s', 'http://p5', True, XSD.boolean.toPython()], # data property with boolean
#     ['http://s', 'http://p6', '48.13188#11.54965#1379714400', iris.GEOSPATIAL_LAT_LON_TIME], # data property with custom datatype
# ]

@pytest.mark.parametrize(
    "s,p,o,data_type,expected_result",
    [
        ('http://s', 'http://p0', 'http://o', None, True), # <http://s> <http://p> <http://o>
        ('http://s', 'http://p1', 'http://o', XSD.string, True), # <http://s> <http://p> "http://o"^^xsd:string
        ('http://s', 'http://p2', 'o', XSD.string.toPython(), True), # <http://s> <http://p> "o"^^xsd:string
        ('http://s', 'http://p3', 3, XSD.int.toPython(), True), # <http://s> <http://p> "3"^^xsd:int
        ('http://s', 'http://p4', 3.14, XSD.double.toPython(), True), # <http://s> <http://p> "3.14"^^xsd:double
        ('http://s', 'http://p5', True, XSD.boolean.toPython(), True), # <http://s> <http://p> "true"^^xsd:boolean
        ('http://s', 'http://p6', '48.13188#11.54965#1379714400', iris.GEOSPATIAL_LAT_LON_TIME, True), # <http://s> <http://p> "48.13188#11.54965#1379714400"^^<http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon-time>
        (None, None, None, None, True), # ?s ?p ?o
        (None, None, 'http://o', None, True), # ?s ?p <http://o>
        (None, None, 'http://o', XSD.string, True), # ?s ?p "http://o"^^xsd:string
        (None, 'http://p0', None, None, True), # ?s <http://p0> ?o
        ('http://s', None, None, None, True), # <http://s> ?p ?o
        (None, 'http://random', None, None, False), # ?s <http://random> ?o
        ('http://random', None, None, None, False), # <http://random> ?p ?o
        (None, None, 'http://random', None, False), # ?s ?p <http://random>
    ],
)
def test_check_if_triple_exist(initialise_test_triples, s, p, o, data_type, expected_result):
    sparql_client = initialise_test_triples
    assert sparql_client.check_if_triple_exist(s, p, o, data_type) == expected_result
