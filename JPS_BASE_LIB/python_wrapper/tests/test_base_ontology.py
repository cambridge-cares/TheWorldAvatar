from __future__ import annotations

import pytest
import uuid
from rdflib import Graph, URIRef, Literal, BNode
from rdflib import OWL, RDF, RDFS, XSD
from typing import ClassVar, ForwardRef

from twa.data_model.base_ontology import BaseOntology, BaseClass, DatatypeProperty, ObjectProperty, TransitiveProperty
from twa.data_model.base_ontology import as_range
from twa.data_model.base_ontology import KnowledgeGraph
from twa.data_model.iris import OWL_BASE_URL


EXAMPLE_BASE_URL = 'https://example.org/'
EXAMPLE_NAMESPACE = 'example'


class ExampleOntology(BaseOntology):
    base_url: ClassVar[str] = EXAMPLE_BASE_URL
    namespace: ClassVar[str] = EXAMPLE_NAMESPACE
    owl_versionInfo: ClassVar[str] = '0.0.1a'
    rdfs_comment: ClassVar[str] = 'An example ontology'


class DataProperty_A(DatatypeProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(str, 0, 1)


class DataProperty_B(DatatypeProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(int, 1, 1)


class Data_Property_C(DatatypeProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(str, 0, 5)


class A(BaseClass):
    is_defined_by_ontology = ExampleOntology
    data_property_a: DataProperty_A

class ObjectProperty_B_A(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(A)


class ObjectProperty_C_A(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(A, 0, 3)


class B(BaseClass):
    is_defined_by_ontology = ExampleOntology
    object_property_b_a: ObjectProperty_B_A
    data_property_b: DataProperty_B


class ObjectProperty_C_B(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(B, 0, 1)


class C(BaseClass):
    is_defined_by_ontology = ExampleOntology
    object_property_c_a: ObjectProperty_C_A
    object_property_c_b: ObjectProperty_C_B
    data_property_c: Data_Property_C


class ObjectProperty_D_C(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(C)


class ObjectProperty_D_B(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(B)


class ObjectProperty_D_A(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(A)


class Data_Property_D(DatatypeProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(str)


class D(BaseClass):
    is_defined_by_ontology = ExampleOntology
    object_property_d_c: ObjectProperty_D_C
    object_property_d_b: ObjectProperty_D_B
    object_property_d_a: ObjectProperty_D_A
    data_property_d: Data_Property_D


class E(D):
    pass


class PartOf(TransitiveProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(Component)


class Component(BaseClass):
    is_defined_by_ontology = ExampleOntology
    partOf: PartOf


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
    assert DataProperty_A.retrieve_cardinality() == (0, 1)
    assert DataProperty_B.retrieve_cardinality() == (1, 1)
    assert Data_Property_C.retrieve_cardinality() == (0, 5)
    assert ObjectProperty_B_A.retrieve_cardinality() == (0, None)
    assert ObjectProperty_C_A.retrieve_cardinality() == (0, 3)
    assert ObjectProperty_C_B.retrieve_cardinality() == (0, 1)


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
    assert BaseClass.get_rdf_type() == OWL_BASE_URL + 'Class'


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
    assert not bool(d._latest_cache) # cache should be empty before push
    d.push_to_kg(sparql_client, -1)
    assert bool(d._latest_cache) # cache should not be empty
    assert sparql_client.get_amount_of_triples() == 18
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
    # check that all cache has also been updated
    assert d._latest_cache['object_property_d_a'].range == set([a1])
    assert d._latest_cache['object_property_d_b'].range == set([b])
    assert d._latest_cache['object_property_d_c'].range == set()
    assert d._latest_cache['data_property_d'].range == set([d_new_str])
    assert c._latest_cache['object_property_c_b'].range == set()
    assert c._latest_cache['object_property_c_a'].range == set([a2])
    assert b._latest_cache['object_property_b_a'].range == set([a3])
    # check that the triples in the KG are correct
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
    # check that all cache has also been updated
    assert d._latest_cache['object_property_d_a'].range == set([a1])
    assert d._latest_cache['object_property_d_b'].range == set()
    assert d._latest_cache['object_property_d_c'].range == set([c])
    assert d._latest_cache['data_property_d'].range == set([new_str])
    assert c._latest_cache['object_property_c_b'].range == set([b])
    assert c._latest_cache['object_property_c_a'].range == set([a2, a3])
    assert b._latest_cache['object_property_b_a'].range == set([a1, a2])
    # check that the triples in the KG are correct
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
    # check that all cache has also been updated
    assert d._latest_cache['object_property_d_a'].range == set([a1])
    assert d._latest_cache['object_property_d_b'].range == set()
    assert d._latest_cache['object_property_d_c'].range == set([c])
    assert d._latest_cache['data_property_d'].range == set([new_str, second_d_new_str])
    assert c._latest_cache['object_property_c_b'].range == set()
    assert c._latest_cache['object_property_c_a'].range == set()
    assert b._latest_cache['object_property_b_a'].range == set()
    # check that the triples in the KG are correct
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


def test_push_when_remote_changed(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    c.push_to_kg(sparql_client, -1)
    a1.push_to_kg(sparql_client, -1)
    # now the remote should have connection c --> a2, a3, b, 'c'
    # manually remove remote connection between c and a2, also add remote connection between c and a1
    # this mimics the operation done by other agents
    sparql_client.perform_update(
        f"""
        delete {{<{c.instance_iri}> <{c.object_property_c_a.predicate_iri}> <{a2.instance_iri}>}}
        insert {{<{c.instance_iri}> <{c.object_property_c_a.predicate_iri}> <{a1.instance_iri}>}}
        where {{}}
        """
    )
    # assert that the previous operation was successful
    assert sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a3.instance_iri)

    # now remove the connection between c and b locally
    c.object_property_c_b.range.remove(b)
    # push to kg again (pull first so that the cache are updated)
    c.push_to_kg(sparql_client, -1, True)
    # check that the triples in the KG are correct
    # i.e. it reflects both manual changes to the remote, as well as the local changes from the python obje
    assert sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_a.predicate_iri, a3.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, c.object_property_c_b.predicate_iri, b.instance_iri)


def test_transitive_property():
    engine_piston = Component()
    engine_valves = Component()
    engine = Component(partOf=[engine_piston, engine_valves])
    headlight_part = Component()
    headlight = Component(partOf=headlight_part)
    wheels = Component()
    car = Component(partOf=[engine, headlight, wheels])
    assert car.partOf.obtain_transitive_objects() == set(
        [engine, headlight, wheels, headlight_part, engine_piston, engine_valves]
    )


def test_revert_local_changes(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    d.push_to_kg(sparql_client, -1)
    assert bool(d._latest_cache) # cache should not be empty
    assert sparql_client.get_amount_of_triples() == 18
    # there shouldn't be any connection between d and b
    assert not bool(d.object_property_d_b.range)
    # add connection between d and b, then revert the changes
    d.object_property_d_b.range.add(b)
    assert bool(d.object_property_d_b.range)
    d.revert_local_changes()
    assert not bool(d.object_property_d_b.range)


# NOTE below we create classes for test_forward_refs_object_property and test_forward_refs_data_property
# NOTE classes are defined before the object/data properties to test the ForwardRef
class F(BaseClass):
    is_defined_by_ontology = ExampleOntology
    fg: Fg

class Fg(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(G)


class G(BaseClass):
    is_defined_by_ontology = ExampleOntology


class FProp(DatatypeProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(str, 0, 1)


from pydantic import create_model
F1 = create_model('F1', fProp=(ForwardRef('F1Prop'), ...), __base__=F)
F2 = create_model('F2', fProp=(ForwardRef('F2Prop'), ...), __base__=F)
F3 = create_model('F3', fProp=(ForwardRef('F3Prop'), ...), __base__=F)

F1Prop = create_model('F1Prop', __base__=FProp)
F2Prop = create_model('F2Prop', __base__=FProp)
F3Prop = create_model('F3Prop', __base__=FProp)


def test_forward_refs_object_property():
    # this test exist to cover the case where classes are defined before the definition of object properties
    # where F.model_fields.items() would appear like:
    # ```dict_items([('fg', FieldInfo(annotation=ForwardRef('Fg'), required=True)), ...])```
    # if `ForwardRef('Fg')` is not properly handled, this will cause the validation error when executing ```F()```:
    # ```
    # E       pydantic_core._pydantic_core.ValidationError: 1 validation error for F
    # E       fg
    # E         Input should be a valid dictionary or instance of Fg [type=model_type, input_value=G(rdfs_comment=None, rdfs...401f-9f5b-009a499b9685'), input_type=G]
    # E           For further information visit https://errors.pydantic.dev/2.6/v/model_type
    # ```
    assert F.model_fields['fg'].annotation == ForwardRef('Fg')
    F(fg=G())
    assert F.model_fields['fg'].annotation == Fg


@pytest.mark.parametrize("clz,prop,arg", [
    (F1, F1Prop, None),
    (F2, F2Prop, 'x'),
    (F3, F3Prop, F3Prop(range='x')),
])
def test_forward_refs_data_property(clz, prop, arg):
    # this test exist to cover the case where classes are defined before the definition of data properties
    # where F1.model_fields.items() would appear like:
    # ```dict_items([('fProp', FieldInfo(annotation=ForwardRef('F1Prop'), required=True)), ...])```
    # if `ForwardRef('F1Prop')` is not properly handled, this will cause the validation error when executing ```F1(fProp='x')```:
    # ```
    # E       pydantic_core._pydantic_core.ValidationError: 1 validation error for F1
    # E       fProp
    # E         Input should be a valid dictionary or instance of F1Prop [type=model_type, input_value='x', input_type=str]
    # E           For further information visit https://errors.pydantic.dev/2.6/v/model_type
    # ```
    assert clz.model_fields['fProp'].annotation == ForwardRef(prop.__name__)
    if arg is None:
        o = clz()
        assert len(o.fProp.range) == 0
    else:
        o = clz(fProp=arg)
        assert len(o.fProp.range) == 1
        assert next(iter(o.fProp.range)) == 'x'
    assert clz.model_fields['fProp'].annotation == prop


# NOTE below we define classes for testing circular graph pattern
class Circ1(BaseClass):
    is_defined_by_ontology = ExampleOntology
    c1c2: C1C2


class C1C2(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(Circ2)


class Circ2(BaseClass):
    is_defined_by_ontology = ExampleOntology
    c2c1: C2C1


class C2C1(ObjectProperty):
    is_defined_by_ontology = ExampleOntology
    range: as_range(Circ1)


def test_pull_circular_graph_pattern(initialise_sparql_client):
    # prepare data
    g = Graph()
    iri_circ_1 = f'https://{str(uuid.uuid4())}'
    iri_circ_2 = f'https://{str(uuid.uuid4())}'
    g.add((URIRef(iri_circ_1), RDF.type, URIRef(Circ1.get_rdf_type())))
    g.add((URIRef(iri_circ_1), URIRef(C1C2.get_predicate_iri()), URIRef(iri_circ_2)))
    g.add((URIRef(iri_circ_2), RDF.type, URIRef(Circ2.get_rdf_type())))
    g.add((URIRef(iri_circ_2), URIRef(C2C1.get_predicate_iri()), URIRef(iri_circ_1)))
    sparql_client = initialise_sparql_client
    sparql_client.upload_graph(g)
    # assert that circ1/circ2 doesn't exist in python
    assert KnowledgeGraph.get_object_from_lookup(iri_circ_1) is None
    assert KnowledgeGraph.get_object_from_lookup(iri_circ_2) is None
    # pull circ2 from knowledge graph
    circ2 = Circ2.pull_from_kg(
        [iri_circ_2],
        sparql_client,
        -1
    )[0]
    # check that circ1 should be pulled as an object
    # but its object property c1c2 should only stored iri of circ2 as string
    assert len(circ2.c2c1.range) == 1
    circ1 = next(iter(circ2.c2c1.range))
    assert circ1.instance_iri == iri_circ_1
    assert len(circ1.c1c2.range) == 1
    assert next(iter(circ1.c1c2.range)) == circ2.instance_iri
    assert isinstance(next(iter(circ1.c1c2.range)), str)


def test_push_circular_graph_pattern(initialise_sparql_client):
    KnowledgeGraph.clear_object_lookup()
    sparql_client = initialise_sparql_client
    # create objects in python
    circ1 = Circ1()
    circ2 = Circ2(c2c1=circ1)
    circ1.c1c2.range.add(circ2.instance_iri)
    # push to triple store
    circ2.push_to_kg(sparql_client, -1)
    # check that the triples are instantiated correctly
    assert sparql_client.get_amount_of_triples() == 4
    assert sparql_client.check_if_triple_exist(circ1.instance_iri, RDF.type.toPython(), Circ1.get_rdf_type())
    assert sparql_client.check_if_triple_exist(circ1.instance_iri, C1C2.get_predicate_iri(), circ2.instance_iri)
    assert sparql_client.check_if_triple_exist(circ2.instance_iri, RDF.type.toPython(), Circ2.get_rdf_type())
    assert sparql_client.check_if_triple_exist(circ2.instance_iri, C2C1.get_predicate_iri(), circ1.instance_iri)


# NOTE this test is put at the end of this test script
# NOTE so that the ForwardRef are not evaluated
# NOTE and therefore can be tested by the tests at the beginning of this file
def test_export_to_graph():
    g = ExampleOntology.export_to_graph()
    print(g.serialize(format='ttl'))

    # object property C1C2, testing for triples:
    # <https://example.org/example/c1C2> a owl:ObjectProperty ;
    #     rdfs:domain <https://example.org/example/Circ1> ;
    #     rdfs:isDefinedBy <https://example.org/example> ;
    #     rdfs:range <https://example.org/example/Circ2> .
    assert (URIRef(C1C2.get_predicate_iri()), RDF.type, OWL.ObjectProperty) in g
    assert (URIRef(C1C2.get_predicate_iri()), RDFS.domain, URIRef(Circ1.get_rdf_type())) in g
    assert (URIRef(C1C2.get_predicate_iri()), RDFS.range, URIRef(Circ2.get_rdf_type())) in g
    assert (URIRef(C1C2.get_predicate_iri()), RDFS.isDefinedBy, URIRef(ExampleOntology.get_namespace_iri())) in g

    # data property Data_Property_D, testing for triples:
    # <https://example.org/example/data_Property_D> a owl:DatatypeProperty ;
    #     rdfs:domain [ a owl:Class ;
    #             owl:unionOf ( <https://example.org/example/D> <https://example.org/example/E> ) ] ;
    #     rdfs:isDefinedBy <https://example.org/example> ;
    #     rdfs:range xsd:string .
    assert (URIRef(Data_Property_D.get_predicate_iri()), RDF.type, OWL.DatatypeProperty) in g
    assert (URIRef(Data_Property_D.get_predicate_iri()), RDFS.range, XSD.string) in g
    assert (URIRef(Data_Property_D.get_predicate_iri()), RDFS.isDefinedBy, URIRef(ExampleOntology.get_namespace_iri())) in g
    assert sum(1 for _ in g.triples((URIRef(Data_Property_D.get_predicate_iri()), RDFS.domain, None))) == 1
    domain_bnode = g.value(URIRef(Data_Property_D.get_predicate_iri()), RDFS.domain)
    assert isinstance(domain_bnode, BNode)
    assert (domain_bnode, RDF.type, OWL.Class) in g
    class_union_bnode = g.value(domain_bnode, OWL.unionOf)
    assert isinstance(class_union_bnode, BNode)
    clz_bnode = g.value(class_union_bnode, RDF.rest)
    assert isinstance(clz_bnode, BNode)
    assert RDF.nil == g.value(clz_bnode, RDF.rest)
    assert set([
        g.value(class_union_bnode, RDF.first).toPython(),
        g.value(clz_bnode, RDF.first).toPython()
    ]) == set([D.get_rdf_type(), E.get_rdf_type()])

    # cardinality constraints, testing for triples:
    # <https://example.org/example/C> a owl:Class ;
    #     rdfs:isDefinedBy <https://example.org/example> ;
    #     rdfs:subClassOf [ a owl:Restriction ;
    #             owl:maxQualifiedCardinality "1"^^xsd:nonNegativeInteger ;
    #             owl:onClass <https://example.org/example/B> ;
    #             owl:onProperty <https://example.org/example/objectProperty_C_B> ],
    #         [ a owl:Restriction ;
    #             owl:maxQualifiedCardinality "3"^^xsd:nonNegativeInteger ;
    #             owl:onClass <https://example.org/example/A> ;
    #             owl:onProperty <https://example.org/example/objectProperty_C_A> ],
    #         [ a owl:Restriction ;
    #             owl:maxQualifiedCardinality "5"^^xsd:nonNegativeInteger ;
    #             owl:onClass xsd:string ;
    #             owl:onProperty <https://example.org/example/data_Property_C> ] .
    # test for C being a class and its properties
    assert (URIRef(C.get_rdf_type()), RDF.type, OWL.Class) in g
    assert (URIRef(C.get_rdf_type()), RDFS.isDefinedBy, URIRef(ExampleOntology.get_namespace_iri())) in g
    assert sum(1 for _ in g.triples((URIRef(C.get_rdf_type()), RDFS.subClassOf, None))) == 3
    # check each subclass restriction
    for restriction in g.objects(URIRef(C.get_rdf_type()), RDFS.subClassOf):
        assert isinstance(restriction, BNode)
        assert (restriction, RDF.type, OWL.Restriction) in g

        max_cardinality = g.value(restriction, OWL.maxQualifiedCardinality)
        on_class = g.value(restriction, OWL.onClass).toPython()
        on_property = g.value(restriction, OWL.onProperty).toPython()

        assert max_cardinality is not None
        assert on_class is not None
        assert on_property is not None

        if max_cardinality == Literal("1", datatype=XSD.nonNegativeInteger):
            assert on_class == B.get_rdf_type()
            assert on_property == ObjectProperty_C_B.get_predicate_iri()
        elif max_cardinality == Literal("3", datatype=XSD.nonNegativeInteger):
            assert on_class == A.get_rdf_type()
            assert on_property == ObjectProperty_C_A.get_predicate_iri()
        elif max_cardinality == Literal("5", datatype=XSD.nonNegativeInteger):
            assert on_class == XSD.string.toPython()
            assert on_property == Data_Property_C.get_predicate_iri()
        else:
            assert False, "Unexpected cardinality or restriction"

def test_graph_and_triples():
    a1, a2, a3, b, c, d = init()
    g = a1.graph() + a2.graph() + a3.graph() + b.graph() + c.graph() + d.graph()

    # 6 triples: instance rdf:type
    assert (URIRef(a1.instance_iri), RDF.type, URIRef(a1.rdf_type)) in g
    assert (URIRef(a2.instance_iri), RDF.type, URIRef(a2.rdf_type)) in g
    assert (URIRef(a3.instance_iri), RDF.type, URIRef(a3.rdf_type)) in g
    assert (URIRef(b.instance_iri), RDF.type, URIRef(b.rdf_type)) in g
    assert (URIRef(c.instance_iri), RDF.type, URIRef(c.rdf_type)) in g
    assert (URIRef(d.instance_iri), RDF.type, URIRef(d.rdf_type)) in g
    # 1 triple: a1 --> 'a1'
    assert (URIRef(a1.instance_iri), URIRef(a1.data_property_a.predicate_iri), Literal("a1")) in g
    # 1 triple: a2 --> 'a2'
    assert (URIRef(a2.instance_iri), URIRef(a2.data_property_a.predicate_iri), Literal("a2")) in g
    # 1 triple: a3 --> 'a3'
    assert (URIRef(a3.instance_iri), URIRef(a3.data_property_a.predicate_iri), Literal("a3")) in g
    # 3 triples: b --> a1, a2, 3
    assert (URIRef(b.instance_iri), URIRef(b.object_property_b_a.predicate_iri), URIRef(a1.instance_iri)) in g
    assert (URIRef(b.instance_iri), URIRef(b.object_property_b_a.predicate_iri), URIRef(a2.instance_iri)) in g
    assert (URIRef(b.instance_iri), URIRef(b.data_property_b.predicate_iri), Literal(3)) in g
    # 4 triples: c --> a2, a3, b, 'c'
    assert (URIRef(c.instance_iri), URIRef(c.object_property_c_a.predicate_iri), URIRef(a2.instance_iri)) in g
    assert (URIRef(c.instance_iri), URIRef(c.object_property_c_a.predicate_iri), URIRef(a3.instance_iri)) in g
    assert (URIRef(c.instance_iri), URIRef(c.object_property_c_b.predicate_iri), URIRef(b.instance_iri)) in g
    assert (URIRef(c.instance_iri), URIRef(c.data_property_c.predicate_iri), Literal("c")) in g
    # 2 triples: d --> a1, c
    assert (URIRef(d.instance_iri), URIRef(d.object_property_d_a.predicate_iri), URIRef(a1.instance_iri)) in g
    assert (URIRef(d.instance_iri), URIRef(d.object_property_d_c.predicate_iri), URIRef(c.instance_iri)) in g

    # in total 18 triples
    assert sum(1 for _ in g.triples((None, None, None))) == 18
