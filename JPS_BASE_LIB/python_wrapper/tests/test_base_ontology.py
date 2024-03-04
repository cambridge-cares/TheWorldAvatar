from rdflib import Graph, RDF, Literal, XSD
from pydantic import Field

from py4jps.data_model.base_ontology import BaseOntology, DataProperty, ObjectProperty
from py4jps.data_model.base_ontology import as_range_of_data_property, as_range_of_object_property


EXAMPLE_BASE_PREFIX = 'https://example.org/'
EXAMPLE_NAMESPACE = 'example'


class ExampleOntology(BaseOntology):
    base_prefix: str = EXAMPLE_BASE_PREFIX
    namespace: str = EXAMPLE_NAMESPACE


class ExampleDataProperty(DataProperty):
    base_prefix: str = EXAMPLE_BASE_PREFIX
    namespace: str = EXAMPLE_NAMESPACE


class ExampleObjectProperty(ObjectProperty):
    base_prefix: str = EXAMPLE_BASE_PREFIX
    namespace: str = EXAMPLE_NAMESPACE


class DataProperty_A(ExampleDataProperty):
    range: as_range_of_data_property(str)


class DataProperty_B(ExampleDataProperty):
    range: as_range_of_data_property(int)


class Data_Property_C(ExampleDataProperty):
    range: as_range_of_data_property(str)


class A(ExampleOntology):
    data_property_a: DataProperty_A = Field(default=None)


class ObjectProperty_B_A(ExampleObjectProperty):
    range: as_range_of_object_property(A)


class ObjectProperty_C_A(ExampleObjectProperty):
    range: as_range_of_object_property(A)


class B(ExampleOntology):
    object_property_b_a: ObjectProperty_B_A = Field(default=None)
    data_property_b: DataProperty_B = Field(default=None)


class ObjectProperty_C_B(ExampleObjectProperty):
    range: as_range_of_object_property(B)


class C(ExampleOntology):
    object_property_c_a: ObjectProperty_C_A = Field(default=None)
    object_property_c_b: ObjectProperty_C_B = Field(default=None)
    data_property_c: Data_Property_C = Field(default=None)


a1 = A(data_property_a=DataProperty_A(range='a1'))
a2 = A(data_property_a=DataProperty_A(range='a2'))
a3 = A(data_property_a=DataProperty_A(range='a3'))
ba = ObjectProperty_B_A(range=[a1, a2, a3])
ca = ObjectProperty_C_A(range=[a1, a2])
b = B(object_property_b_a=ba, data_property_b=DataProperty_B(range=3))
cb = ObjectProperty_C_B(range=[b])
c = C(object_property_c_a=ca, object_property_c_b=cb,
        data_property_c=Data_Property_C(range='c'))


def test_rdf_type():
    a = BaseOntology()
    assert a.rdf_type is not None
    assert BaseOntology.get_rdf_type() == a.rdf_type


def test_create_triples_for_kg():
    g = Graph()
    g = b.create_triples_for_kg(g)
    # instance rdf:type
    assert g.query(f'ASK {{<{b.instance_iri}> <{RDF.type.toPython()}> <{b.rdf_type}>}}').askAnswer
    # data property
    assert g.query(f'ASK {{<{b.instance_iri}> <{b.data_property_b.predicate_iri}> {3}}}').askAnswer
    # object property
    assert g.query(f'ASK {{<{b.instance_iri}> <{b.object_property_b_a.predicate_iri}> <{a1.instance_iri}>}}').askAnswer
    assert g.query(f'ASK {{<{b.instance_iri}> <{b.object_property_b_a.predicate_iri}> <{a2.instance_iri}>}}').askAnswer
    assert g.query(f'ASK {{<{b.instance_iri}> <{b.object_property_b_a.predicate_iri}> <{a3.instance_iri}>}}').askAnswer
    # a1/a2/a3 instance rdf:type
    assert g.query(f'ASK {{<{a1.instance_iri}> <{RDF.type.toPython()}> <{a1.rdf_type}>}}').askAnswer
    assert g.query(f'ASK {{<{a2.instance_iri}> <{RDF.type.toPython()}> <{a2.rdf_type}>}}').askAnswer
    assert g.query(f'ASK {{<{a3.instance_iri}> <{RDF.type.toPython()}> <{a3.rdf_type}>}}').askAnswer
    # a1/a2/a3 data property
    assert g.query(f'ASK {{<{a1.instance_iri}> <{a1.data_property_a.predicate_iri}> "a1"}}').askAnswer
    assert g.query(f'ASK {{<{a2.instance_iri}> <{a2.data_property_a.predicate_iri}> "a2"}}').askAnswer
    assert g.query(f'ASK {{<{a3.instance_iri}> <{a3.data_property_a.predicate_iri}> "a3"}}').askAnswer
    # total number of triples
    res = g.query('SELECT (COUNT(*) AS ?c) WHERE {?s ?p ?o}')
    for row in res:
        assert row.c == Literal(11)


def test_push_to_kg(initialise_sparql_client):
    sparql_client = initialise_sparql_client
    assert sparql_client.get_amount_of_triples() == 0
    b.push_to_kg(sparql_client)
    assert sparql_client.get_amount_of_triples() == 11
    # instance rdf:type
    assert sparql_client.check_if_triple_exist(b.instance_iri, RDF.type.toPython(), b.rdf_type)
    # data property
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.data_property_b.predicate_iri, 3, XSD.integer.toPython())
    # object property
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a1.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a3.instance_iri)
    # a1/a2/a3 instance rdf:type
    assert sparql_client.check_if_triple_exist(a1.instance_iri, RDF.type.toPython(), a1.rdf_type)
    assert sparql_client.check_if_triple_exist(a2.instance_iri, RDF.type.toPython(), a2.rdf_type)
    assert sparql_client.check_if_triple_exist(a3.instance_iri, RDF.type.toPython(), a3.rdf_type)
    # a1/a2/a3 data property
    assert sparql_client.check_if_triple_exist(a1.instance_iri, a1.data_property_a.predicate_iri, "a1", XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(a2.instance_iri, a2.data_property_a.predicate_iri, "a2", XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(a3.instance_iri, a3.data_property_a.predicate_iri, "a3", XSD.string.toPython())


def test_pull_from_kg(initialise_sparql_client):
    sparql_client = initialise_sparql_client
    c.push_to_kg(sparql_client)
    assert A.pull_from_kg(a1.instance_iri, sparql_client)[0] == a1
    assert A.pull_from_kg(a2.instance_iri, sparql_client)[0] == a2
    assert A.pull_from_kg(a3.instance_iri, sparql_client)[0] == a3
    assert B.pull_from_kg(b.instance_iri, sparql_client, -1)[0] == b
    assert C.pull_from_kg(c.instance_iri, sparql_client, -1)[0] == c
    ba_str = ObjectProperty_B_A(range=[a1.instance_iri, a2.instance_iri, a3.instance_iri])
    _b = B(object_property_b_a=ba_str, data_property_b=DataProperty_B(range=3))
    cb_str = ObjectProperty_C_B(range=[_b])
    _c = C(object_property_c_a=ca, object_property_c_b=cb_str,
            data_property_c=Data_Property_C(range='c'))
    assert B.pull_from_kg(b.instance_iri, sparql_client, 0)[0] == _b
    # when recursive depth is 1, C instance should be pulled with A instances
    # but its B instance should not be fully populated, i.e., B's A instances should not be pulled
    assert C.pull_from_kg(c.instance_iri, sparql_client, 1)[0] == _c
