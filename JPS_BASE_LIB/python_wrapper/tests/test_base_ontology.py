from typing import List, Optional
import rdflib
from pydantic import Field

from py4jps.data_model.base_ontology import BaseOntology, DataProperty, ObjectProperty

from py4jps.kg_operations import PySparqlClient


EXAMPLE_BASE_PREFIX = 'http://example.org/'
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
    object: str

class DataProperty_B(ExampleDataProperty):
    object: int

class Data_Property_C(ExampleDataProperty):
    object: str

class A(ExampleOntology):
    data_property_a: Optional[DataProperty_A] = Field(default=None)

class ObjectProperty_B_A(ExampleObjectProperty):
    object: List[A]

class ObjectProperty_C_A(ExampleObjectProperty):
    object: List[A]

class B(ExampleOntology):
    object_property_b_a: Optional[ObjectProperty_B_A]
    data_property_b: Optional[DataProperty_B]

class ObjectProperty_C_B(ExampleObjectProperty):
    object: B

class C(ExampleOntology):
    object_property_c_a: Optional[ObjectProperty_C_A]
    object_property_c_b: Optional[ObjectProperty_C_B]
    data_property_c: Optional[Data_Property_C]

def test_create_triples_for_kg(initialise_triple_store):
    sparql_endpoint = initialise_triple_store
    a1 = A(data_property_a=DataProperty_A(object='a1'))
    a2 = A(data_property_a=DataProperty_A(object='a2'))
    a3 = A(data_property_a=DataProperty_A(object='a3'))
    ba = ObjectProperty_B_A(object=[a1, a2, a3])
    ca = ObjectProperty_C_A(object=[a1, a2])
    b = B(object_property_b_a=ba, data_property_b=DataProperty_B(object=3))
    cb = ObjectProperty_C_B(object=b)
    c = C(object_property_c_a=ca, object_property_c_b=cb, data_property_c=Data_Property_C(object='c'))
    # sparql_endpoint = 'http://localhost:48082/blazegraph/namespace/pydantic/sparql'
    sparql_client = PySparqlClient(sparql_endpoint, sparql_endpoint)
    sparql_client.perform_update('delete where { ?s ?p ?o }')
    print(c)
    c.push_to_kg(sparql_client)

def test_push_to_kg():
    pass

def test_pull_from_kg():
    pass
