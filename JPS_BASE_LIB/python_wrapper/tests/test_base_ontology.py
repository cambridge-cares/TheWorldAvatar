from __future__ import annotations

import pytest
import uuid
from rdflib import Graph, URIRef, Literal, BNode
from rdflib import OWL, RDF, RDFS, XSD
from typing import ClassVar, ForwardRef, Optional

from pydantic_core._pydantic_core import ValidationError

from twa.data_model.base_ontology import BaseOntology, BaseClass, DatatypeProperty, ObjectProperty, TransitiveProperty
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
    rdfs_isDefinedBy = ExampleOntology
    owl_minQualifiedCardinality = 0
    owl_maxQualifiedCardinality = 1


DataProperty_B = DatatypeProperty.create_from_base('DataProperty_B', ExampleOntology, 1, 1)
Data_Property_C = DatatypeProperty.create_from_base('Data_Property_C', ExampleOntology, 0, 5)


class A(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    data_property_a: Optional[DataProperty_A[str]] = None


ObjectProperty_B_A = ObjectProperty.create_from_base('ObjectProperty_B_A', ExampleOntology)
ObjectProperty_C_A = ObjectProperty.create_from_base('ObjectProperty_C_A', ExampleOntology, 0, 3)


class B(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    object_property_b_a: Optional[ObjectProperty_B_A[A]] = None
    data_property_b: DataProperty_B[int]


ObjectProperty_C_B = ObjectProperty.create_from_base('ObjectProperty_C_B', ExampleOntology, 0, 1)


class C(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    object_property_c_a: ObjectProperty_C_A[A]
    object_property_c_b: ObjectProperty_C_B[B]
    data_property_c: Data_Property_C[str]


ObjectProperty_D_C = ObjectProperty.create_from_base('ObjectProperty_D_C', ExampleOntology)
ObjectProperty_D_B = ObjectProperty.create_from_base('ObjectProperty_D_B', ExampleOntology)
ObjectProperty_D_A = ObjectProperty.create_from_base('ObjectProperty_D_A', ExampleOntology)
Data_Property_D = DatatypeProperty.create_from_base('Data_Property_D', ExampleOntology)


class D(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    object_property_d_c: ObjectProperty_D_C[C]
    object_property_d_b: Optional[ObjectProperty_D_B[B]] = set()
    object_property_d_a: ObjectProperty_D_A[A]
    data_property_d: Optional[Data_Property_D[str]] = set()


class E(D):
    pass


class HasPart(TransitiveProperty):
    rdfs_isDefinedBy = ExampleOntology


class Component(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    hasPart: Optional[HasPart[Component]] = None


def init():
    KnowledgeGraph.clear_object_lookup()
    # 3 triple: a1 --> 'a1', 'a1 comment', 'a1 label'
    a1 = A(data_property_a={'a1'}, rdfs_comment='a1 comment', rdfs_label='a1 label')
    # 1 triple: a2 --> 'a2'
    a2 = A(data_property_a={'a2'})
    # 1 triple: a3 --> 'a3'
    a3 = A(data_property_a={'a3'})
    # 3 triples: b --> a1, a2, 3
    b = B(object_property_b_a=[a1, a2], data_property_b={3})
    # 4 triples: c --> a2, a3, b, 'c'
    c = C(object_property_c_a=[a2, a3], object_property_c_b=[b], data_property_c={'c'})
    # 2 triples: d --> a1, c
    d = D(object_property_d_a=[a1], object_property_d_c=[c])
    # and 6 rdf:type triples
    # in total 20 triples
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
        assert cls == KnowledgeGraph.class_lookup[cls.rdf_type]

    # property registration
    for prop in [DataProperty_A, DataProperty_B, Data_Property_C, Data_Property_D,
                ObjectProperty_B_A, ObjectProperty_C_A, ObjectProperty_C_B,
                ObjectProperty_D_A, ObjectProperty_D_B, ObjectProperty_D_C]:
        assert prop == KnowledgeGraph.property_lookup[prop.predicate_iri]

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


def test_basics():
    # able to generate the json schema without exception
    assert bool(D.model_json_schema())

    # test create object
    a = A(data_property_a={'a'}, rdfs_comment='my comment', rdfs_label='my label')
    assert a.data_property_a == {'a'}
    assert a.rdfs_isDefinedBy.base_url in a.instance_iri
    assert a.rdfs_comment == 'my comment'
    assert a.rdfs_label == 'my label'
    # test one can instantiate with a custom instance_iri
    my_random_iri = f'https://{str(uuid.uuid4())}'
    a_with_random_iri = A(data_property_a={'a'}, instance_iri=my_random_iri)
    assert a_with_random_iri.instance_iri == my_random_iri

    # test create nested object
    b = B(object_property_b_a=[a], data_property_b={1})
    assert b.object_property_b_a == {a}
    assert b.data_property_b == {1}
    aa = A(data_property_a={'aa'})

    # test create nested object - missing optional/required field
    # object_property_b_a is optional with default None
    bb = B(data_property_b={1})
    assert bb.object_property_b_a == None
    # data_property_b is required but not provided
    with pytest.raises(ValidationError) as e_info:
        bb = B(object_property_b_a=[a])
    assert e_info.match('1 validation error for B')
    assert e_info.match('data_property_b')
    assert e_info.match('Field required')
    """
    an example:
    E           pydantic_core._pydantic_core.ValidationError: 1 validation error for B
    E           data_property_b
    E             Field required [type=missing, input_value={'object_property_b_a': [...data_property_a={'a'})]}, input_type=dict]
    E               For further information visit https://errors.pydantic.dev/2.8/v/missing
    """

    # test add to set
    b.object_property_b_a.add(aa)
    assert b.object_property_b_a == {a, aa}
    b.data_property_b.add(2)
    assert b.data_property_b == {1, 2}

    # test remove from set
    b.object_property_b_a.remove(a)
    assert b.object_property_b_a == {aa}
    b.data_property_b.remove(1)
    assert b.data_property_b == {2}

    # test set to new value
    b.object_property_b_a = {a}
    assert b.object_property_b_a == {a}
    b.data_property_b = {1}
    assert b.data_property_b == {1}

    # test clear set
    b.object_property_b_a.clear()
    assert b.object_property_b_a == set()
    b.data_property_b.clear()
    assert b.data_property_b == set()

    # test set to None
    # object_property_b_a can be set to None as it's optional
    b.object_property_b_a = None
    assert b.object_property_b_a == None
    # data_property_b cannot be set to None as it's not optional
    with pytest.raises(ValidationError) as e_info:
        b.data_property_b = None
    assert e_info.match('1 validation error for B')
    assert e_info.match('data_property_b')
    assert e_info.match('Input should be a valid set')
    """
    an example:
    E           pydantic_core._pydantic_core.ValidationError: 1 validation error for B
    E           data_property_b
    E             Input should be a valid set [type=set_type, input_value=None, input_type=NoneType]
    E               For further information visit https://errors.pydantic.dev/2.8/v/set_type
    """


def test_rdf_type():
    a_exp = A()
    assert a_exp.rdf_type is not None
    assert A.rdf_type == a_exp.rdf_type
    assert BaseClass.rdf_type == OWL_BASE_URL + 'Class'


def test_collect_diff_to_graph_fresh():
    # test if the freshly initialised instances are correctly added to the graph
    a1, a2, a3, b, c, d = init()
    g_to_remove = Graph()
    g_to_add = Graph()
    g_to_remove, g_to_add = b.collect_diff_to_graph(g_to_remove, g_to_add, -1)
    # b instance rdf:type
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{RDF.type.toPython()}> <{b.rdf_type}>}}').askAnswer
    # data property
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{DataProperty_B.predicate_iri}> {3}}}').askAnswer
    # b-a1/a2 object property
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{ObjectProperty_B_A.predicate_iri}> <{a1.instance_iri}>}}').askAnswer
    assert g_to_add.query(f'ASK {{<{b.instance_iri}> <{ObjectProperty_B_A.predicate_iri}> <{a2.instance_iri}>}}').askAnswer
    # a1/a2 instance rdf:type
    assert g_to_add.query(f'ASK {{<{a1.instance_iri}> <{RDF.type.toPython()}> <{a1.rdf_type}>}}').askAnswer
    assert g_to_add.query(f'ASK {{<{a2.instance_iri}> <{RDF.type.toPython()}> <{a2.rdf_type}>}}').askAnswer
    # a1/a2 data property
    assert g_to_add.query(f'ASK {{<{a1.instance_iri}> <{DataProperty_A.predicate_iri}> "a1"}}').askAnswer
    assert g_to_add.query(f'ASK {{<{a2.instance_iri}> <{DataProperty_A.predicate_iri}> "a2"}}').askAnswer
    # a1 rdfs_comment and rdfs_label
    assert g_to_add.query(f'ASK {{<{a1.instance_iri}> <{RDFS.comment.toPython()}> "a1 comment"}}').askAnswer
    assert g_to_add.query(f'ASK {{<{a1.instance_iri}> <{RDFS.label.toPython()}> "a1 label"}}').askAnswer
    # total number of triples
    res = g_to_add.query('SELECT (COUNT(*) AS ?con) WHERE {?s ?p ?o}')
    for row in res:
        assert row.con == Literal(10)
    res = g_to_remove.query('SELECT (COUNT(*) AS ?con) WHERE {?s ?p ?o}')
    for row in res:
        assert row.con == Literal(0)


def test_push_to_kg_fresh(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    assert sparql_client.get_amount_of_triples() == 0
    b.push_to_kg(sparql_client, -1)
    assert sparql_client.get_amount_of_triples() == 10
    # instance rdf:type
    assert sparql_client.check_if_triple_exist(b.instance_iri, RDF.type.toPython(), b.rdf_type)
    # data property
    assert sparql_client.check_if_triple_exist(b.instance_iri, DataProperty_B.predicate_iri, 3, XSD.integer.toPython())
    # b-a1/a2 object property
    assert sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a1.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a2.instance_iri)
    # a1/a2 instance rdf:type
    assert sparql_client.check_if_triple_exist(a1.instance_iri, RDF.type.toPython(), a1.rdf_type)
    assert sparql_client.check_if_triple_exist(a2.instance_iri, RDF.type.toPython(), a2.rdf_type)
    # a1/a2 data property
    assert sparql_client.check_if_triple_exist(a1.instance_iri, DataProperty_A.predicate_iri, "a1", XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(a2.instance_iri, DataProperty_A.predicate_iri, "a2", XSD.string.toPython())


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
    assert sparql_client.get_amount_of_triples() == 20
    d_pulled = D.pull_from_kg(d.instance_iri, sparql_client, -1)[0]
    # the d_pulled should be pointing to the same memory location as d
    # but its _latest_cache should be updated - previously it was empty
    assert id(d_pulled) == id(d)
    assert bool(d_pulled._latest_cache)

    # change the python objects to:
    # d --> a1, b, 'd changed'
    d.object_property_d_b.add(b) # +1
    d.object_property_d_c.remove(c) # -1
    d_new_str = 'd changed ' + str(uuid.uuid4())
    d.data_property_d.add(d_new_str) # +1
    # c --> a2
    c.object_property_c_b.remove(b) # -1
    c.object_property_c_a.remove(a3) # -1
    # b --> a3
    b.object_property_b_a.remove(a1) # -1
    b.object_property_b_a.remove(a2) # -1
    b.object_property_b_a.add(a3) # +1
    # in total: 20 + 1 - 1 + 1 - 1 - 1 - 1 - 1 + 1 = 18
    # the cache should look the same as the original
    # NOTE the set operation taking the hash of the object, so only IRI is checked (not the object/data properties)
    assert d._latest_cache['object_property_d_a'] == set([a1])
    assert d._latest_cache['object_property_d_b'] == set()
    assert d._latest_cache['object_property_d_c'] == set([c])
    assert d._latest_cache['data_property_d'] == set()
    # as the cache only contains the connection between the objects, the range should be strings
    _cached_c_in_d = next(iter(d._latest_cache['object_property_d_c']))
    assert isinstance(_cached_c_in_d, str)
    assert c._latest_cache['object_property_c_b'] == set([b])
    assert c._latest_cache['object_property_c_a'] == set([a2, a3])
    assert b._latest_cache['object_property_b_a'] == set([a1, a2])
    g_to_remove = Graph()
    g_to_add = Graph()
    g_to_remove, g_to_add = d.collect_diff_to_graph(g_to_remove, g_to_add, -1) # recursive push -1
    print("g_to_remove:")
    print(g_to_remove.serialize(format='turtle'))
    print("g_to_add:")
    print(g_to_add.serialize(format='turtle'))
    d.push_to_kg(sparql_client, -1) # recursive push -1
    # check that all cache has also been updated
    assert d._latest_cache['object_property_d_a'] == set([a1])
    assert d._latest_cache['object_property_d_b'] == set([b])
    assert d._latest_cache['object_property_d_c'] == set()
    assert d._latest_cache['data_property_d'] == set([d_new_str])
    assert c._latest_cache['object_property_c_b'] == set()
    assert c._latest_cache['object_property_c_a'] == set([a2])
    assert b._latest_cache['object_property_b_a'] == set([a3])
    # check that the triples in the KG are correct
    assert sparql_client.get_amount_of_triples() == 18
    assert not sparql_client.check_if_triple_exist(d.instance_iri, ObjectProperty_D_C.predicate_iri, c.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, Data_Property_D.predicate_iri, d_new_str, XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(d.instance_iri, ObjectProperty_D_B.predicate_iri, b.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_B.predicate_iri, b.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a3.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a3.instance_iri)

    # now change the python objects back:
    # c --> a2, a3, b
    c.object_property_c_a.add(a3) # +1
    c.object_property_c_b.add(b) # +1
    # b --> a1, a2
    b.object_property_b_a.add(a1) # +1
    b.object_property_b_a.add(a2) # +1
    b.object_property_b_a.remove(a3) # -1
    # d --> a1, c, 'd changed', 'd changed again'
    new_str = 'd changed again ' + str(uuid.uuid4())
    d.object_property_d_b.remove(b) # -1
    d.object_property_d_c.add(c) # +1
    d.data_property_d.add(new_str) # +1
    d.data_property_d.remove(d_new_str) # -1
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
    assert d._latest_cache['object_property_d_a'] == set([a1])
    assert d._latest_cache['object_property_d_b'] == set()
    assert d._latest_cache['object_property_d_c'] == set([c])
    assert d._latest_cache['data_property_d'] == set([new_str])
    assert c._latest_cache['object_property_c_b'] == set([b])
    assert c._latest_cache['object_property_c_a'] == set([a2, a3])
    assert b._latest_cache['object_property_b_a'] == set([a1, a2])
    # check that the triples in the KG are correct
    assert sparql_client.get_amount_of_triples() == 21
    assert not sparql_client.check_if_triple_exist(d.instance_iri, ObjectProperty_D_B.predicate_iri, b.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, ObjectProperty_D_C.predicate_iri, c.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, Data_Property_D.predicate_iri, new_str, XSD.string.toPython())
    assert not sparql_client.check_if_triple_exist(d.instance_iri, Data_Property_D.predicate_iri, d_new_str, XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a3.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_B.predicate_iri, b.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a1.instance_iri)
    assert sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a2.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a3.instance_iri)

    # now if we keep the connection between d and c unchanged
    # but change the connection between c and b, and b and a, and c and a
    b.object_property_b_a.remove(a1) # -1
    b.object_property_b_a.remove(a2) # -1
    c.object_property_c_a.remove(a2) # -1
    c.object_property_c_a.remove(a3) # -1
    c.object_property_c_b.remove(b) # -1
    second_d_new_str = 'd has more than one data property ' + str(uuid.uuid4())
    d.data_property_d.add(second_d_new_str) # +1
    # also remove the rdfs_comment and rdfs_label from a1
    a1.rdfs_comment = None # -1
    a1.rdfs_label = None # -1
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
    assert d._latest_cache['object_property_d_a'] == set([a1])
    assert d._latest_cache['object_property_d_b'] == set()
    assert d._latest_cache['object_property_d_c'] == set([c])
    assert d._latest_cache['data_property_d'] == set([new_str, second_d_new_str])
    assert c._latest_cache['object_property_c_b'] == set()
    assert c._latest_cache['object_property_c_a'] == set()
    assert b._latest_cache['object_property_b_a'] == set()
    # check that the triples in the KG are correct
    assert sparql_client.get_amount_of_triples() == 15
    assert not sparql_client.check_if_triple_exist(d.instance_iri, ObjectProperty_D_B.predicate_iri, b.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, ObjectProperty_D_C.predicate_iri, c.instance_iri)
    assert sparql_client.check_if_triple_exist(d.instance_iri, Data_Property_D.predicate_iri, new_str, XSD.string.toPython())
    assert not sparql_client.check_if_triple_exist(d.instance_iri, Data_Property_D.predicate_iri, d_new_str, XSD.string.toPython())
    assert sparql_client.check_if_triple_exist(d.instance_iri, Data_Property_D.predicate_iri, second_d_new_str, XSD.string.toPython())
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a2.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a3.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_B.predicate_iri, b.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(b.instance_iri, ObjectProperty_B_A.predicate_iri, a2.instance_iri)


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
        delete {{<{c.instance_iri}> <{ObjectProperty_C_A.predicate_iri}> <{a2.instance_iri}>}}
        insert {{<{c.instance_iri}> <{ObjectProperty_C_A.predicate_iri}> <{a1.instance_iri}>}}
        where {{}}
        """
    )
    # assert that the previous operation was successful
    assert sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a3.instance_iri)

    # now remove the connection between c and b locally
    c.object_property_c_b.remove(b)
    # push to kg again (pull first so that the cache are updated)
    c.push_to_kg(sparql_client, -1, True)
    # check that the triples in the KG are correct
    # i.e. it reflects both manual changes to the remote, as well as the local changes from the python obje
    assert sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a1.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a2.instance_iri)
    assert sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_A.predicate_iri, a3.instance_iri)
    assert not sparql_client.check_if_triple_exist(c.instance_iri, ObjectProperty_C_B.predicate_iri, b.instance_iri)


def test_transitive_property():
    engine_piston = Component(rdfs_label='engine piston')
    engine_valves = Component(rdfs_label='engine valves')
    engine = Component(hasPart=[engine_piston, engine_valves], rdfs_label='engine')
    headlight_part = Component(rdfs_label='headlight part')
    headlight = Component(hasPart=[headlight_part], rdfs_label='headlight')
    wheels = Component(rdfs_label='wheels')
    car = Component(hasPart=[engine, headlight, wheels], rdfs_label='car')
    assert len(HasPart.obtain_transitive_objects(car)) == 6
    assert HasPart.obtain_transitive_objects(car) == set(
        [engine, headlight, wheels, headlight_part, engine_piston, engine_valves]
    )


def test_revert_local_changes(initialise_sparql_client):
    a1, a2, a3, b, c, d = init()
    sparql_client = initialise_sparql_client
    d.push_to_kg(sparql_client, -1)
    assert bool(d._latest_cache) # cache should not be empty
    assert sparql_client.get_amount_of_triples() == 20
    # there shouldn't be any connection between d and b
    assert not bool(d.object_property_d_b)
    # add connection between d and b, then revert the changes
    d.object_property_d_b.add(b)
    assert bool(d.object_property_d_b)
    d.revert_local_changes()
    assert not bool(d.object_property_d_b)


# NOTE below we create classes for test_forward_refs_object_property and test_forward_refs_data_property
# NOTE classes are defined before the object/data properties to test the ForwardRef
class F(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    fg: Optional[Fg[G]] = set()


Fg = ObjectProperty.create_from_base('Fg', ExampleOntology)


class G(BaseClass):
    rdfs_isDefinedBy = ExampleOntology


FProp = DatatypeProperty.create_from_base('FProp', ExampleOntology, 0, 1)


from pydantic import create_model
F1 = create_model('F1', fProp=(ForwardRef('Optional[F1Prop[str]]'), set()), __base__=F)
F2 = create_model('F2', fProp=(ForwardRef('F2Prop[str]'), ...), __base__=F)
F3 = create_model('F3', fProp=(ForwardRef('F3Prop[str]'), ...), __base__=F)


F1Prop = DatatypeProperty.create_from_base('F1Prop', ExampleOntology, base_property=FProp)
F2Prop = DatatypeProperty.create_from_base('F2Prop', ExampleOntology, min_cardinality=1, base_property=FProp)
F3Prop = DatatypeProperty.create_from_base('F3Prop', ExampleOntology, max_cardinality=5, base_property=FProp)


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
    assert F.model_fields['fg'].annotation == ForwardRef('Optional[Fg[G]]')
    F(fg=[G()])
    assert F.model_fields['fg'].annotation == Optional[Fg[G]]


@pytest.mark.parametrize("clz,prop,arg,rdfs_range,optional", [
    (F1, F1Prop, None, str, True),
    (F2, F2Prop, ['x'], str, False),
    (F3, F3Prop, F3Prop('x'), str, False),
])
def test_forward_refs_data_property(clz, prop, arg, rdfs_range, optional):
    # this test exist to cover the case where classes are defined before the definition of data properties
    # where F1.model_fields.items() would appear like:
    # ```dict_items([('fProp', FieldInfo(annotation=ForwardRef('F1Prop[str]'), required=True)), ...])```
    # if `ForwardRef('F1Prop[str]')` is not properly handled, this will cause the validation error when executing ```F1(fProp='x')```:
    # ```
    # E       pydantic_core._pydantic_core.ValidationError: 1 validation error for F1
    # E       fProp
    # E         Input should be a valid dictionary or instance of F1Prop [type=model_type, input_value='x', input_type=str]
    # E           For further information visit https://errors.pydantic.dev/2.6/v/model_type
    # ```
    assert clz.model_fields['fProp'].annotation == ForwardRef(f'Optional[{prop.__name__}[{rdfs_range.__name__}]]') if optional else ForwardRef(f'{prop.__name__}[{rdfs_range.__name__}]')
    if arg is None:
        o = clz()
        assert len(o.fProp) == 0
    else:
        o = clz(fProp=arg)
        assert len(o.fProp) == 1
        assert next(iter(o.fProp)) == 'x'
    assert clz.model_fields['fProp'].annotation == Optional[prop[rdfs_range]] if optional else prop[rdfs_range]


Fg1 = ObjectProperty.create_from_base('Fg1', ExampleOntology, base_property=Fg)
Fg2 = ObjectProperty.create_from_base('Fg2', ExampleOntology, min_cardinality=1, base_property=Fg)
Fg3 = ObjectProperty.create_from_base('Fg3', ExampleOntology, max_cardinality=5, base_property=Fg)


def test_subclassing_object_property():
    # base property should be in the mro
    assert Fg in Fg1.__mro__
    assert Fg in Fg2.__mro__
    assert Fg in Fg3.__mro__
    # cardinality should be inherited correctly
    assert Fg1.retrieve_cardinality() == (0, None)
    assert Fg2.retrieve_cardinality() == (1, None)
    assert Fg3.retrieve_cardinality() == (0, 5)
    # predicate_iri should be set different from its base
    assert Fg1.predicate_iri != Fg.predicate_iri
    assert Fg2.predicate_iri != Fg.predicate_iri
    assert Fg3.predicate_iri != Fg.predicate_iri


def test_subclassing_data_property():
    # base property should be in the mro
    assert FProp in F1Prop.__mro__
    assert FProp in F2Prop.__mro__
    assert FProp in F3Prop.__mro__
    # cardinality should be inherited correctly
    assert F1Prop.retrieve_cardinality() == (0, 1)
    assert F2Prop.retrieve_cardinality() == (1, 1)
    assert F3Prop.retrieve_cardinality() == (0, 5)
    # predicate_iri should be set different from its base
    assert F1Prop.predicate_iri != FProp.predicate_iri
    assert F2Prop.predicate_iri != FProp.predicate_iri
    assert F3Prop.predicate_iri != FProp.predicate_iri


def test_subclassing_transitive_property():
    # base property should be in the mro
    assert TransitiveProperty in HasPart.__mro__
    # predicate_iri should be set different from its base
    assert HasPart.predicate_iri != TransitiveProperty.predicate_iri


# NOTE below we define classes for testing circular graph pattern
C1C2 = ObjectProperty.create_from_base('C1C2', ExampleOntology)


class Circ1(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    c1c2: Optional[C1C2[Circ2]] = set()


class Circ2(BaseClass):
    rdfs_isDefinedBy = ExampleOntology
    c2c1: C2C1[Circ1]


C2C1 = ObjectProperty.create_from_base('C2C1', ExampleOntology)


def test_pull_circular_graph_pattern(initialise_sparql_client):
    # prepare data
    g = Graph()
    iri_circ_1 = f'https://{str(uuid.uuid4())}'
    iri_circ_2 = f'https://{str(uuid.uuid4())}'
    g.add((URIRef(iri_circ_1), RDF.type, URIRef(Circ1.rdf_type)))
    g.add((URIRef(iri_circ_1), URIRef(C1C2.predicate_iri), URIRef(iri_circ_2)))
    g.add((URIRef(iri_circ_2), RDF.type, URIRef(Circ2.rdf_type)))
    g.add((URIRef(iri_circ_2), URIRef(C2C1.predicate_iri), URIRef(iri_circ_1)))
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
    assert len(circ2.c2c1) == 1
    circ1 = next(iter(circ2.c2c1))
    assert circ1.instance_iri == iri_circ_1
    assert len(circ1.c1c2) == 1
    assert next(iter(circ1.c1c2)) == circ2.instance_iri
    assert isinstance(next(iter(circ1.c1c2)), str)


def test_push_circular_graph_pattern(initialise_sparql_client):
    KnowledgeGraph.clear_object_lookup()
    sparql_client = initialise_sparql_client
    # create objects in python
    circ1 = Circ1()
    circ2 = Circ2(c2c1=[circ1])
    circ1.c1c2.add(circ2.instance_iri)
    # push to triple store
    circ2.push_to_kg(sparql_client, -1)
    # check that the triples are instantiated correctly
    assert sparql_client.get_amount_of_triples() == 4
    assert sparql_client.check_if_triple_exist(circ1.instance_iri, RDF.type.toPython(), Circ1.rdf_type)
    assert sparql_client.check_if_triple_exist(circ1.instance_iri, C1C2.predicate_iri, circ2.instance_iri)
    assert sparql_client.check_if_triple_exist(circ2.instance_iri, RDF.type.toPython(), Circ2.rdf_type)
    assert sparql_client.check_if_triple_exist(circ2.instance_iri, C2C1.predicate_iri, circ1.instance_iri)


# NOTE this test is put at the end of this test script
# NOTE so that the ForwardRef are not evaluated
# NOTE and therefore can be tested by the tests at the beginning of this file
def test_export_to_graph(recwarn):
    g = ExampleOntology.export_to_graph()
    assert len(recwarn) == 8
    warning_messages = [str(w.message) for w in recwarn]
    for c in [Fg1, Fg2, Fg3, FProp]:
        assert f'Warning: property {c} has no domain to be added, i.e. it is not used by any classes!' in warning_messages
        assert f'Warning: property {c} has no range to be added, i.e. it is not used by any classes!' in warning_messages

    print(g.serialize(format='ttl'))

    # object property C1C2, testing for triples:
    # <https://example.org/example/c1C2> a owl:ObjectProperty ;
    #     rdfs:domain <https://example.org/example/Circ1> ;
    #     rdfs:isDefinedBy <https://example.org/example> ;
    #     rdfs:range <https://example.org/example/Circ2> .
    assert (URIRef(C1C2.predicate_iri), RDF.type, OWL.ObjectProperty) in g
    assert (URIRef(C1C2.predicate_iri), RDFS.domain, URIRef(Circ1.rdf_type)) in g
    assert (URIRef(C1C2.predicate_iri), RDFS.range, URIRef(Circ2.rdf_type)) in g
    assert (URIRef(C1C2.predicate_iri), RDFS.isDefinedBy, URIRef(ExampleOntology.get_namespace_iri())) in g

    # data property Data_Property_D, testing for triples:
    # <https://example.org/example/data_Property_D> a owl:DatatypeProperty ;
    #     rdfs:domain [ a owl:Class ;
    #             owl:unionOf ( <https://example.org/example/D> <https://example.org/example/E> ) ] ;
    #     rdfs:isDefinedBy <https://example.org/example> ;
    #     rdfs:range xsd:string .
    assert (URIRef(Data_Property_D.predicate_iri), RDF.type, OWL.DatatypeProperty) in g
    assert (URIRef(Data_Property_D.predicate_iri), RDFS.range, XSD.string) in g
    assert (URIRef(Data_Property_D.predicate_iri), RDFS.isDefinedBy, URIRef(ExampleOntology.get_namespace_iri())) in g
    assert sum(1 for _ in g.triples((URIRef(Data_Property_D.predicate_iri), RDFS.domain, None))) == 1
    domain_bnode = g.value(URIRef(Data_Property_D.predicate_iri), RDFS.domain)
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
    ]) == set([D.rdf_type, E.rdf_type])

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
    assert (URIRef(C.rdf_type), RDF.type, OWL.Class) in g
    assert (URIRef(C.rdf_type), RDFS.isDefinedBy, URIRef(ExampleOntology.get_namespace_iri())) in g
    assert sum(1 for _ in g.triples((URIRef(C.rdf_type), RDFS.subClassOf, None))) == 3
    # check each subclass restriction
    for restriction in g.objects(URIRef(C.rdf_type), RDFS.subClassOf):
        assert isinstance(restriction, BNode)
        assert (restriction, RDF.type, OWL.Restriction) in g

        max_cardinality = g.value(restriction, OWL.maxQualifiedCardinality)
        on_class = g.value(restriction, OWL.onClass).toPython()
        on_property = g.value(restriction, OWL.onProperty).toPython()

        assert max_cardinality is not None
        assert on_class is not None
        assert on_property is not None

        if max_cardinality == Literal("1", datatype=XSD.nonNegativeInteger):
            assert on_class == B.rdf_type
            assert on_property == ObjectProperty_C_B.predicate_iri
        elif max_cardinality == Literal("3", datatype=XSD.nonNegativeInteger):
            assert on_class == A.rdf_type
            assert on_property == ObjectProperty_C_A.predicate_iri
        elif max_cardinality == Literal("5", datatype=XSD.nonNegativeInteger):
            assert on_class == XSD.string.toPython()
            assert on_property == Data_Property_C.predicate_iri
        else:
            assert False, "Unexpected cardinality or restriction"


def test_graph_and_triples():
    a1, a2, a3, b, c, d = init()
    # KnowledgeGraph.graph() should be equivalent to:
    # a1.graph() + a2.graph() + a3.graph() + b.graph() + c.graph() + d.graph()
    g = KnowledgeGraph.graph()

    # 6 triples: instance rdf:type
    assert (URIRef(a1.instance_iri), RDF.type, URIRef(a1.rdf_type)) in g
    assert (URIRef(a2.instance_iri), RDF.type, URIRef(a2.rdf_type)) in g
    assert (URIRef(a3.instance_iri), RDF.type, URIRef(a3.rdf_type)) in g
    assert (URIRef(b.instance_iri), RDF.type, URIRef(b.rdf_type)) in g
    assert (URIRef(c.instance_iri), RDF.type, URIRef(c.rdf_type)) in g
    assert (URIRef(d.instance_iri), RDF.type, URIRef(d.rdf_type)) in g
    # 1 triple: a1 --> 'a1'
    assert (URIRef(a1.instance_iri), URIRef(DataProperty_A.predicate_iri), Literal("a1")) in g
    # 1 triple: a2 --> 'a2'
    assert (URIRef(a2.instance_iri), URIRef(DataProperty_A.predicate_iri), Literal("a2")) in g
    # 1 triple: a3 --> 'a3'
    assert (URIRef(a3.instance_iri), URIRef(DataProperty_A.predicate_iri), Literal("a3")) in g
    # 3 triples: b --> a1, a2, 3
    assert (URIRef(b.instance_iri), URIRef(ObjectProperty_B_A.predicate_iri), URIRef(a1.instance_iri)) in g
    assert (URIRef(b.instance_iri), URIRef(ObjectProperty_B_A.predicate_iri), URIRef(a2.instance_iri)) in g
    assert (URIRef(b.instance_iri), URIRef(DataProperty_B.predicate_iri), Literal(3)) in g
    # 4 triples: c --> a2, a3, b, 'c'
    assert (URIRef(c.instance_iri), URIRef(ObjectProperty_C_A.predicate_iri), URIRef(a2.instance_iri)) in g
    assert (URIRef(c.instance_iri), URIRef(ObjectProperty_C_A.predicate_iri), URIRef(a3.instance_iri)) in g
    assert (URIRef(c.instance_iri), URIRef(ObjectProperty_C_B.predicate_iri), URIRef(b.instance_iri)) in g
    assert (URIRef(c.instance_iri), URIRef(Data_Property_C.predicate_iri), Literal("c")) in g
    # 2 triples: d --> a1, c
    assert (URIRef(d.instance_iri), URIRef(ObjectProperty_D_A.predicate_iri), URIRef(a1.instance_iri)) in g
    assert (URIRef(d.instance_iri), URIRef(ObjectProperty_D_C.predicate_iri), URIRef(c.instance_iri)) in g

    # in total 20 triples
    assert sum(1 for _ in g.triples((None, None, None))) == 20


def test_all_triples_of_nodes():
    a1, a2, a3, b, c, d = init()
    g = KnowledgeGraph.all_triples_of_nodes(a1.instance_iri)
    # 1 triple: instance rdf:type
    assert (URIRef(a1.instance_iri), RDF.type, URIRef(a1.rdf_type)) in g
    # 1 triple: a1 --> 'a1'
    assert (URIRef(a1.instance_iri), URIRef(DataProperty_A.predicate_iri), Literal("a1")) in g
    # 1 triple: a1 --> 'a1 comment'
    assert (URIRef(a1.instance_iri), URIRef(RDFS.comment), Literal("a1 comment")) in g
    # 1 triple: a1 --> 'a1 label'
    assert (URIRef(a1.instance_iri), URIRef(RDFS.label), Literal("a1 label")) in g
    # 1 triple: b --> a1
    assert (URIRef(b.instance_iri), URIRef(ObjectProperty_B_A.predicate_iri), URIRef(a1.instance_iri)) in g
    # 1 triple: d --> a1
    assert (URIRef(d.instance_iri), URIRef(ObjectProperty_D_A.predicate_iri), URIRef(a1.instance_iri)) in g
    # in total 6 triples
    assert sum(1 for _ in g.triples((None, None, None))) == 6
