from typing import List, Optional
from pydantic import model_validator
import rdflib
from pydantic import Field

from py4jps.data_model.base_ontology import BaseOntology, DataProperty, ObjectProperty
from py4jps.data_model.base_ontology import as_range_of_data_property, as_range_of_object_property

from py4jps.kg_operations import PySparqlClient


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

    @model_validator(mode='before')
    def val_obj(cls, data):
        print("before========================================================")
        print(data)
        print("========================================================")
        return data

    @model_validator(mode='after')
    def validate_object(self):
        print("after========================================================")
        print(self)
        print(self.range)
        import time
        # #time.sleep(100)
        if not bool(self.range):
            raise ValueError("Object property C_A must have at least one object.")
        return self

class B(ExampleOntology):
    object_property_b_a: ObjectProperty_B_A = Field(default=None)
    data_property_b: DataProperty_B = Field(default=None)

class ObjectProperty_C_B(ExampleObjectProperty):
    range: as_range_of_object_property(B)

class C(ExampleOntology):
    object_property_c_a: ObjectProperty_C_A = Field(default=None)
    object_property_c_b: ObjectProperty_C_B = Field(default=None)
    data_property_c: Data_Property_C = Field(default=None)

def test_():
    a = BaseOntology()
    print(a.rdf_type)
    print(BaseOntology.get_rdf_type())
    # print(BaseOntology.rdf_type)

def test_create_triples_for_kg():#initialise_triple_store):
    # sparql_endpoint = initialise_triple_store
    a1 = A(data_property_a=DataProperty_A(range='a1'))
    a2 = A(data_property_a=DataProperty_A(range='a2'))
    a3 = A(data_property_a=DataProperty_A(range='a3'))
    import time
    print(a1)
    #time.sleep(2)
    print(a2)
    #time.sleep(2)
    print(a3)
    #time.sleep(2)
    ba = ObjectProperty_B_A(range=[a1, a2, a3])
    ca = ObjectProperty_C_A(range=[a1, a2])
    print("-------------------")
    print(ca)
    print("-------------------")
    b = B(object_property_b_a=ba, data_property_b=DataProperty_B(range=3))
    cb = ObjectProperty_C_B(range=[b])
    c = C(object_property_c_a=ca, object_property_c_b=cb, data_property_c=Data_Property_C(range='c'))
    sparql_endpoint = 'http://localhost:48082/blazegraph/namespace/pydantic/sparql'
    sparql_client = PySparqlClient(sparql_endpoint, sparql_endpoint)
    sparql_client.perform_update('delete where { ?s ?p ?o }')
    print(a1.rdf_type)
    print(b.rdf_type)
    print(c.rdf_type)
    print(BaseOntology().rdf_type)
    # print(c)
    c.push_to_kg(sparql_client)
    print(a1.instance_iri)
    print(a1.instance_iri)
    print(a1.instance_iri)
    a1_pulled = A.pull_from_kg(a1.instance_iri, sparql_client)
    print(a1_pulled)
    print("************************************************************************")
    print(C.pull_from_kg(c.instance_iri, sparql_client))
    print("************************************************************************")
    print(c)
    print("************************************************************************")
    print(c.dict())
    print("************************************************************************")
    # print(C.get_iri_dict())
    print("************************************************************************")
    print('a1 dict:', a1.dict())
    print('a1:', [a1])
    print('a1_pulled:', a1_pulled)
    print(a1_pulled)

def test_push_to_kg():
    pass

def test_pull_from_kg():
    pass
