from __future__ import annotations

import pytest
import uuid

from rdflib import Graph, RDF, Literal, XSD
from pydantic import Field
from typing import ClassVar, Set

from py4jps.data_model.base_ontology import BaseOntology, BaseClass, DataProperty, ObjectProperty
from py4jps.data_model.base_ontology import as_range_of_data_property, as_range_of_object_property
from py4jps.data_model.base_ontology import KnowledgeGraph


EXAMPLE_BASE_URL = 'https://example.org/'
EXAMPLE_NAMESPACE = 'example'


class ExampleOntology(BaseOntology):
    base_url: ClassVar[str] = EXAMPLE_BASE_URL
    namespace: ClassVar[str] = EXAMPLE_NAMESPACE
    owl_versionInfo: ClassVar[str] = '0.0.1a'
    rdfs_comment: ClassVar[str] = 'An example ontology'


class DataProperty_A(DataProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_data_property(str, 0, None)


class DataProperty_B(DataProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_data_property(int, 0, 1)


class Data_Property_C(DataProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_data_property(str, 0, 5)


class A(BaseClass):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    data_property_a: DataProperty_A = Field(default_factory=DataProperty_A)


class ObjectProperty_B_A(ObjectProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_object_property(A, 0, None)


class ObjectProperty_C_A(ObjectProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_object_property(A, 0, 3)


class B(BaseClass):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    object_property_b_a: ObjectProperty_B_A = Field(default_factory=ObjectProperty_B_A)
    data_property_b: DataProperty_B = Field(default_factory=DataProperty_B)


class ObjectProperty_C_B(ObjectProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_object_property(B, 0, 1)


class C(BaseClass):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    object_property_c_a: ObjectProperty_C_A = Field(default_factory=ObjectProperty_C_A)
    object_property_c_b: ObjectProperty_C_B = Field(default_factory=ObjectProperty_C_B)
    data_property_c: Data_Property_C = Field(default_factory=Data_Property_C)


class ObjectProperty_D_C(ObjectProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_object_property(C)


class ObjectProperty_D_B(ObjectProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_object_property(B)


class ObjectProperty_D_A(ObjectProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_object_property(A)


class Data_Property_D(DataProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    range: as_range_of_data_property(str)


class D(BaseClass):
    is_defined_by_ontology: ClassVar[BaseOntology] = ExampleOntology
    object_property_d_c: ObjectProperty_D_C = Field(default_factory=ObjectProperty_D_C)
    object_property_d_b: ObjectProperty_D_B = Field(default_factory=ObjectProperty_D_B)
    object_property_d_a: ObjectProperty_D_A = Field(default_factory=ObjectProperty_D_A)
    data_property_d: Data_Property_D = Field(default_factory=Data_Property_D)

class E(D):
    pass


def init():
    KnowledgeGraph.clear_object_lookup()
    # 1 triple: a1 --> 'a1'
    a1 = A(data_property_a='a1')
    # 1 triple: a2 --> 'a2'
    a2 = A(data_property_a='a2')
    # 1 triple: a3 --> 'a3'
    a3 = A(data_property_a='a3')
    # 3 triples: b --> a1, a2, 3
    b = B(object_property_b_a=[a1, a2], data_property_b=3)
    # 4 triples: c --> a2, a3, b, 'c'
    c = C(object_property_c_a=[a2, a3], object_property_c_b=[b], data_property_c='c')
    # 2 triples: d --> a1, c
    d = D(object_property_d_a=[a1], object_property_d_c=[c])
    # and 6 rdf:type triples
    # in total 18 triples
    return a1, a2, a3, b, c, d


def test_retrieve_cardinality():
    assert DataProperty_A.retrieve_cardinality() == (0, None)
    assert DataProperty_B.retrieve_cardinality() == (0, 1)
    assert Data_Property_C.retrieve_cardinality() == (0, 5)
    assert ObjectProperty_B_A.retrieve_cardinality() == (0, None)
    assert ObjectProperty_C_A.retrieve_cardinality() == (0, 3)
    assert ObjectProperty_C_B.retrieve_cardinality() == (0, 1)
    # TODO handle min cardinality of 1


def test_export_to_owl():
    # TODO add more tests to the exported file
    ExampleOntology.export_to_owl('example_ontology.ttl')


def test_register_and_clear():
    # class registration
    for cls in [A, B, C, D]:
        assert cls == KnowledgeGraph.class_lookup[cls.get_rdf_type()]

    # property registration
    for prop in [DataProperty_A, DataProperty_B, Data_Property_C, Data_Property_D,
                ObjectProperty_B_A, ObjectProperty_C_A, ObjectProperty_C_B,
                ObjectProperty_D_A, ObjectProperty_D_B, ObjectProperty_D_C]:
        assert prop == KnowledgeGraph.property_lookup[prop.get_predicate_iri()]

    # object registration
    assert not bool(KnowledgeGraph.construct_object_lookup())
    init()
    for cls in [A, B, C, D]:
        for obj_iri, obj in cls.object_lookup.items():
            assert obj_iri in KnowledgeGraph.construct_object_lookup()
            assert obj == KnowledgeGraph.construct_object_lookup()[obj_iri]
    assert len(A.object_lookup) == 3
    assert len(B.object_lookup) == 1
    assert len(C.object_lookup) == 1
    assert len(D.object_lookup) == 1

    # clear object lookup
    KnowledgeGraph.clear_object_lookup()
    assert not bool(KnowledgeGraph.construct_object_lookup())
    for cls in [A, B, C, D]:
        assert not bool(cls.object_lookup)


def test_added_to_domain():
    a1, a2, a3, b, c, d = init()
    for p in [DataProperty_A]:
        assert set([A.get_rdf_type()]) == p.domain
    for p in [DataProperty_B, ObjectProperty_B_A]:
        assert set([B.get_rdf_type()]) == p.domain
    for p in [Data_Property_C, ObjectProperty_C_A, ObjectProperty_C_B]:
        assert set([C.get_rdf_type()]) == p.domain
    for p in [Data_Property_D, ObjectProperty_D_A, ObjectProperty_D_B, ObjectProperty_D_C]:
        assert set([D.get_rdf_type(), E.get_rdf_type()]) == p.domain


def test_rdf_type():
    a_exp = A()
    assert a_exp.rdf_type is not None
    assert A.get_rdf_type() == a_exp.rdf_type


def test_collect_diff_to_graph_fresh():
    # test if the freshly initialised instances are correctly added to the graph
    a1, a2, a3, b, c, d = init()
    g_to_remove = Graph()
    g_to_add = Graph()
    g_to_remove, g_to_add = b.collect_diff_to_graph(g_to_remove, g_to_add, -1)
    # b instance rdf:type
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{RDF.type.toPython()}> <{b.rdf_type}>}}').askAnswer
    # data property
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{b.data_property_b.predicate_iri}> {3}}}').askAnswer
    # b-a1/a2 object property
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{b.object_property_b_a.predicate_iri}> <{a1.instance_iri}>}}').askAnswer
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{b.object_property_b_a.predicate_iri}> <{a2.instance_iri}>}}').askAnswer
    # a1/a2 instance rdf:type
    assert g_to_add.query(f'ASK {{<{a1.instance_iri}> <{RDF.type.toPython()}> <{a1.rdf_type}>}}').askAnswer
    assert g_to_add.query(f'ASK {{<{a2.instance_iri}> <{RDF.type.toPython()}> <{a2.rdf_type}>}}').askAnswer
    # a1/a2 data property
    assert g_to_add.query(f'ASK {{<{a1.instance_iri}> <{a1.data_property_a.predicate_iri}> "a1"}}').askAnswer
    assert g_to_add.query(f'ASK {{<{a2.instance_iri}> <{a2.data_property_a.predicate_iri}> "a2"}}').askAnswer
    # total number of triples
    res = g_to_add.query('SELECT (COUNT(*) AS ?con) WHERE {?s ?p ?o}')
    for row in res:
        assert row.con == Literal(8)
    res = g_to_remove.query('SELECT (COUNT(*) AS ?con) WHERE {?s ?p ?o}')
    for row in res:
        assert row.con == Literal(0)


def test_push_to_kg_fresh(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    assert sparql_client.get_amount_of_triples() == 0
    b.push_to_kg(sparql_client, -1)
    assert sparql_client.get_amount_of_triples() == 8
    # instance rdf:type
    assert sparql_client.check_if_triple_exist(b.instance_iri, RDF.type.toPython(), b.rdf_type)
    # data property
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.data_property_b.predicate_iri, 3, XSD.integer.toPython())
    # b-a1/a2 object property
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a1.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a2.instance_iri)
    # a1/a2 instance rdf:type
    assert sparql_client.check_if_triple_exist(a1.instance_iri, RDF.type.toPython(), a1.rdf_type)
    assert sparql_client.check_if_triple_exist(a2.instance_iri, RDF.type.toPython(), a2.rdf_type)
    # a1/a2 data property
    assert sparql_client.check_if_triple_exist(a1.instance_iri, a1.data_property_a.predicate_iri, "a1", XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(a2.instance_iri, a2.data_property_a.predicate_iri, "a2", XSD.string.toPython())


def test_pull_from_kg(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    c.push_to_kg(sparql_client, -1)
    # id of the object pulled should be the same as the one that was pushed to the KG
    # because the object was already registered in the ontology object lookup when initialised
    assert id(C.pull_from_kg(c.instance_iri, sparql_client, -1)[0]) == id(c)
    # after clearing the ontology object lookup, the object should be pulled from the KG again
    # therefore the id of the object should be different
    KnowledgeGraph.clear_object_lookup()
    assert A.pull_from_kg(a1.instance_iri, sparql_client)[0] == a1
    assert A.pull_from_kg(a2.instance_iri, sparql_client)[0] == a2
    assert A.pull_from_kg(a3.instance_iri, sparql_client)[0] == a3
    assert B.pull_from_kg(b.instance_iri, sparql_client, -1)[0] == b
    assert C.pull_from_kg(c.instance_iri, sparql_client, -1)[0] == c
    assert id(C.pull_from_kg(c.instance_iri, sparql_client, -1)[0]) != id(c)
    # test exception when pulling an object that does not match the type of the class
    with pytest.raises(ValueError) as e_info:
        A.pull_from_kg(c.instance_iri, sparql_client)
    print(e_info)


def test_pull_all_instance_from_kg(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    c.push_to_kg(sparql_client, -1)
    a_list = A.pull_all_instances_from_kg(sparql_client)
    assert len(a_list) == 3
    for a_pulled in a_list:
        assert a_pulled in [a1, a2, a3]
    assert B.pull_all_instances_from_kg(sparql_client) == [b]
    c_pulled_list = C.pull_all_instances_from_kg(sparql_client, -1)
    assert c_pulled_list == [c]


def test_push_to_kg_update(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    assert sparql_client.get_amount_of_triples() == 0
    d.push_to_kg(sparql_client, -1)
    assert sparql_client.get_amount_of_triples() == 18
    assert not bool(d._latest_cache) # cache should be empty
    d_pulled = D.pull_from_kg(d.instance_iri, sparql_client, -1)[0]
    # the d_pulled should be pointing to the same memory location as d
    # but its _latest_cache should be updated - previously it was empty
    assert id(d_pulled) == id(d)
    assert bool(d_pulled._latest_cache)

    # change the python objects to:
    # d --> a1, b, 'd changed'
    d.object_property_d_b.range.add(b) # +1
    d.object_property_d_c.range.remove(c) # -1
    d_new_str = 'd changed ' + str(uuid.uuid4())
    d.data_property_d.range.add(d_new_str) # +1
    # c --> a2
    c.object_property_c_b.range.remove(b) # -1
    c.object_property_c_a.range.remove(a3) # -1
    # b --> a3
    b.object_property_b_a.range.remove(a1) # -1
    b.object_property_b_a.range.remove(a2) # -1
    b.object_property_b_a.range.add(a3) # +1
    # in total: 18 + 1 - 1 + 1 - 1 - 1 - 1 - 1 + 1 = 16
    # the cache should look the same as the original
    # NOTE the set operation taking the hash of the object, so only IRI is checked (not the object/data properties)
    assert d._latest_cache['object_property_d_a'].range == set([a1])
    assert d._latest_cache['object_property_d_b'].range == set()
    assert d._latest_cache['object_property_d_c'].range == set([c])
    assert d._latest_cache['data_property_d'].range == set()
    # as the cache only contains the connection between the objects, the range should be strings
    _cached_c_in_d = next(iter(d._latest_cache['object_property_d_c'].range))
    assert isinstance(_cached_c_in_d, str)
    assert c._latest_cache['object_property_c_b'].range == set([b])
    assert c._latest_cache['object_property_c_a'].range == set([a2, a3])
    assert b._latest_cache['object_property_b_a'].range == set([a1, a2])
    g_to_remove = Graph()
    g_to_add = Graph()
    g_to_remove, g_to_add = d.collect_diff_to_graph(g_to_remove, g_to_add, -1) # recursive push -1
    print("g_to_remove:")
    print(g_to_remove.serialize(format='turtle'))
    print("g_to_add:")
    print(g_to_add.serialize(format='turtle'))
    d.push_to_kg(sparql_client, -1) # recursive push -1
    assert sparql_client.get_amount_of_triples() == 16
    assert not sparql_client.check_if_triple_exist(d.instance_iri, d.object_property_d_c.predicate_iri, c.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, d.data_property_d.predicate_iri, d_new_str, XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(d.instance_iri, d.object_property_d_b.predicate_iri, b.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_b.predicate_iri, b.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a3.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a3.instance_iri)

    # now change the python objects back:
    # c --> a2, a3, b
    c.object_property_c_a.range.add(a3) # +1
    c.object_property_c_b.range.add(b) # +1
    # b --> a1, a2
    b.object_property_b_a.range.add(a1) # +1
    b.object_property_b_a.range.add(a2) # +1
    b.object_property_b_a.range.remove(a3) # -1
    # d --> a1, c, 'd changed', 'd changed again'
    new_str = 'd changed again ' + str(uuid.uuid4())
    d.object_property_d_b.range.remove(b) # -1
    d.object_property_d_c.range.add(c) # +1
    d.data_property_d.range.add(new_str) # +1
    d.data_property_d.range.remove(d_new_str) # -1
    # update the cache
    D.pull_from_kg(d.instance_iri, sparql_client, -1)
    # print the diff
    g_to_remove = Graph()
    g_to_add = Graph()
    g_to_remove, g_to_add = d.collect_diff_to_graph(g_to_remove, g_to_add, -1) # recursive push -1
    print("g_to_remove:")
    print(g_to_remove.serialize(format='turtle'))
    print("g_to_add:")
    print(g_to_add.serialize(format='turtle'))
    # push the changes to the KG
    d.push_to_kg(sparql_client, -1)
    assert sparql_client.get_amount_of_triples() == 19
    assert not sparql_client.check_if_triple_exist(d.instance_iri, d.object_property_d_b.predicate_iri, b.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, d.object_property_d_c.predicate_iri, c.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, d.data_property_d.predicate_iri, new_str, XSD.string.toPython())
    assert not sparql_client.check_if_triple_exist(d.instance_iri, d.data_property_d.predicate_iri, d_new_str, XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a3.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_b.predicate_iri, b.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a1.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a2.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a3.instance_iri)

    # now if we keep the connection between d and c unchanged
    # but change the connection between c and b, and b and a, and c and a
    b.object_property_b_a.range.remove(a1) # -1
    b.object_property_b_a.range.remove(a2) # -1
    c.object_property_c_a.range.remove(a2) # -1
    c.object_property_c_a.range.remove(a3) # -1
    c.object_property_c_b.range.remove(b) # -1
    second_d_new_str = 'd has more than one data property ' + str(uuid.uuid4())
    d.data_property_d.range.add(second_d_new_str) # +1
    # update the cache
    D.pull_from_kg(d.instance_iri, sparql_client, -1)
    # print the diff
    g_to_remove = Graph()
    g_to_add = Graph()
    g_to_remove, g_to_add = d.collect_diff_to_graph(g_to_remove, g_to_add, -1) # recursive push -1
    print("g_to_remove:")
    print(g_to_remove.serialize(format='turtle'))
    print("g_to_add:")
    print(g_to_add.serialize(format='turtle'))
    # push the changes to the KG
    d.push_to_kg(sparql_client, -1)
    assert sparql_client.get_amount_of_triples() == 15
    assert not sparql_client.check_if_triple_exist(d.instance_iri, d.object_property_d_b.predicate_iri, b.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, d.object_property_d_c.predicate_iri, c.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, d.data_property_d.predicate_iri, new_str, XSD.string.toPython())
    assert not sparql_client.check_if_triple_exist(d.instance_iri, d.data_property_d.predicate_iri, d_new_str, XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(d.instance_iri, d.data_property_d.predicate_iri, second_d_new_str, XSD.string.toPython())
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a2.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a3.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_b.predicate_iri, b.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, b.object_property_b_a.predicate_iri, a2.instance_iri)
