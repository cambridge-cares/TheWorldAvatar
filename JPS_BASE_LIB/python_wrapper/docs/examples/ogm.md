`twa` package provides an implementation of Object Graph Mapper (OGM) using [Pydantic](https://github.com/pydantic/pydantic) to model the objects in Python memory which provides type validation, as well as [rdflib](https://github.com/RDFLib/rdflib) to host the objects in their triple format which can then be connected to a triple store using `twa.PySparqlClient`.

Below we provide minimal working example of how to use the OGM.

## TBox level


### Define an ontology (in Pydantic)

To begin with, you can define the ontology that hosts all concepts and relationships as below:

```python
# Import relevant packages
from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
from twa.data_model.iris import TWA_BASE_URL
from typing import ClassVar
from pydantic import Field

# Your ontology needs to inherit the BaseOntology class
class YourOntology(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url: ClassVar[str] = TWA_BASE_URL
    namespace: ClassVar[str] = 'yourontology'
    owl_versionInfo: ClassVar[str] = '0.0.1'
    rdfs_comment: ClassVar[str] = 'Your ontology'
    # Since they are already defined as a ClassVar[str], one can just assign value to it
    # i.e., simplified version:
    # ```
    # base_url = TWA_BASE_URL
    # namespace = 'yourontology'
    # owl_versionInfo = '0.0.1'
    # rdfs_comment = 'Your ontology'
    # ```
```

which is equivalent to the below triples in OWL:

```turtle
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<https://www.theworldavatar.com/kg/yourontology> a owl:Ontology ;
    rdfs:comment "Your ontology" ;
    owl:versionInfo "0.0.1" .
```

For simplicity, `<https://www.theworldavatar.com/kg/yourontology/>` (**Note the `/` at the end!!!**) will be replaced as prefix `yo` in the rest of this page:
```turtle
@prefix yo: <https://www.theworldavatar.com/kg/yourontology/> .
```

### Define a property (relationship)

To define custom object and data properties, the two base classes `ObjectProperty` and `DatatypeProperty` should be used respectively. It should be noted that the user is only required to specify the cardinality of these properties at the class defination, as their `rdfs:domain` and `rdfs:range` will be automatically handled by the class that utilises the defined properties.


#### Object property

To define a custom object property:

```python
PointsToAnotherConcept = ObjectProperty.create_from_base(
    class_name = 'PointsToAnotherConcept', # The name of the class that will be created and can be directly accessed in code
    ontology = YourOntology, # The user MUST provide the ontology for which the concept `rdfs_isDefinedBy`
    min_cardinality = 0, # 0 is the default value, indicates no cardinality restriction (this arg can be omitted)
    max_cardinality = None, # None is the default value, indicates no cardinality restriction (this arg can be omitted)
)
```

The above definition is equivalent to the below if one would like to follow the typical way of defining a class:

```python
class PointsToAnotherConcept(ObjectProperty):
    rdfs_isDefinedBy = YourOntology # `rdfs_isDefinedBy` can be directly assigned a value here

    # 0 and None in the previous cell indicate no cardinality restriction
    # They are also the default value so you can omitted them here
    # However, if you do want to provide such value then follow the below two lines:
    # ```
    # owl_minQualifiedCardinality = 0
    # owl_maxQualifiedCardinality = None
    # ```
```

which is equivalent to the below triples in OWL:

```turtle
yo:PointsToAnotherConcept a owl:ObjectProperty ;
    rdfs:domain yo:OneConcept ;
    rdfs:isDefinedBy <https://www.theworldavatar.com/kg/yourontology> ;
    rdfs:range yo:AnotherConcept .
```

To access the IRI of the defined objective property:

```python
PointsToAnotherConcept.predicate_iri
```

To define a property that is subproperty of another property:

```python
AnExampleOfSubProperty = PointsToAnotherConcept.create_from_base(
    class_name = 'AnExampleOfSubProperty',
    ontology = YourOntology,
    min_cardinality = 3, # if this value is provided then it overwrites the value from the calling class `PointsToAnotherConcept`
    max_cardinality = 5, # if this value is provided then it overwrites the value from the calling class `PointsToAnotherConcept`
)
```

Further subclassing by calling `AnExampleOfSubProperty.create_from_base(...)` is also possible.

> NOTE that the statements about `rdfs:domain` and `rdfs:range` will be automatically added when defining concept that uses this object property, e.g. assume `OneConcept` uses this object property on `AnotherConcept`, then we have triples: ```yo:PointsToAnotherConcept rdfs:domain yo:OneConcept ; rdfs:range yo:AnotherConcept .```
>
> NOTE for multiple concepts as domain of the same object property, e.g. assume both `OneConcept` and `SubConcept` are the domain, then a [Blank Node](https://www.w3.org/TR/turtle/#BNodes) of `owl:Class` will be added: ```yo:pointsToAnotherConcept rdfs:domain [ a owl:Class ; owl:unionOf ( yo:OneConcept yo:SubConcept ) ] ;```


##### Transitive property
Transitive property is a specific type of object property, it can be very useful for representing [part-whole relations](https://www.w3.org/2001/sw/BestPractices/OEP/SimplePartWhole/). To define a custom transitive property:

```python

OneTransitiveProperty = TransitiveProperty.create_from_base('OneTransitiveProperty', YourOntology)
# Equivalent to:
# ```
# class OneTransitiveProperty(TransitiveProperty):
#     rdfs_isDefinedBy = YourOntology
# ```

# Here we also provide the class definition for the concept that makes use of `OneTransitiveProperty`
# Please refer to later part of this documentation for more examples on how to define a class
class OneClassWithTransitive(BaseClass):
    rdfs_isDefinedBy = YourOntology
    # Note that here the range and domain of `OneTransitiveProperty` are both `OneClassWithTransitive`
    oneTransitiveProperty: OneTransitiveProperty[OneClassWithTransitive]
```

To access the IRI of the defined transitive property:

```python
OneClassWithTransitive.predicate_iri
```

A convenient member function is provided to retrieve all transitive objects as a [`set`](https://docs.python.org/3/tutorial/datastructures.html#sets):

```python
# Assume we have instantiated an object `one_class_with_transitive` of class `OneClassWithTransitive`
# As it uses transitive property `OneTransitiveProperty`, we can retrieve a set of transtive objects by:
set_of_transitive_objects = OneTransitiveProperty.obtain_transitive_objects()
```


#### Data property

To define a custom data property:

```python
OneDatatypeProperty = DatatypeProperty.create_from_base(
    'OneDatatypeProperty', YourOntology
)

AnotherDatatypeProperty = DatatypeProperty.create_from_base(
    'AnotherDatatypeProperty', YourOntology, 0, 1
    # The cardinality means maximum 1
)
```

which is equivalent to the below triples in OWL:

```turtle
yo:OneDatatypeProperty a owl:DatatypeProperty ;
    rdfs:domain yo:OneConcept ;
    rdfs:isDefinedBy <https://www.theworldavatar.com/kg/yourontology> ;
    rdfs:range xsd:string .

yo:AnotherDatatypeProperty a owl:DatatypeProperty ;
    rdfs:domain yo:AnotherConcept ;
    rdfs:isDefinedBy <https://www.theworldavatar.com/kg/yourontology> ;
    rdfs:range xsd:integer .

yo:AnotherConcept rdfs:subClassOf [
    a owl:Restriction ;
    owl:maxQualifiedCardinality "1"^^xsd:nonNegativeInteger ;
    owl:onClass xsd:integer ;
    owl:onProperty yo:AnotherDatatypeProperty ] .
```

To access the IRI of the defined datatype property:

```python
OneDatatypeProperty.predicate_iri
```

> NOTE that the cardinality for `AnotherDatatypeProperty` will be added as a [Blank Node](https://www.w3.org/TR/turtle/#BNodes) of `owl:Restriction` automatically to `AnotherConcept` when the class is defined.


### Define a class (concept)

The classes can be defined in the normal way as defining Python native classes, with the field being the object/data properties previously defined:

```python
class OneConcept(BaseClass):
    # Like object/data properties, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy = YourOntology
    # Follow format `myDatatypeProperty: MyDatatypeProperty[str]`
    oneDatatypeProperty: OneDatatypeProperty[str]
    # Follow format `myObjectProperty: MyObjectProperty[MyOtherClass]`
    pointsToAnotherConcept: PointsToAnotherConcept[AnotherConcept]

class AnotherConcept(BaseClass):
    rdfs_isDefinedBy = YourOntology
    anotherDatatypeProperty: AnotherDatatypeProperty[int]
```

To access the `rdf:type` of the defined class:
```python
OneConcept.rdf_type
```

> NOTE that the name of field **CAN NOT** be the same as the name of the corresponding Pydantic class it is referring to, i.e. `AnotherDatatypeProperty: AnotherDatatypeProperty` would be invalid.


#### Class and subclass

The subclass relationship `rdfs:subClassOf` can also be defined following the standard practice, e.g. we define a concept `SubConcept` `rdfs:subClassOf` `OneConcept`:

```python
AdditionalDatatypeProperty = DatatypeProperty.create_from_base(
    'AdditionalDatatypeProperty', YourOntology, 0, 1
)

class SubConcept(OneConcept):
    # As it inherits `OneConcept`, only additional object/data properties are required
    additionalDatatypeProperty: AdditionalDatatypeProperty[int]
```


#### Multiple inheritance

Multiple inheritance is also possible:

```python
YetAnotherDatatypeProperty = DatatypeProperty.create_from_base(
    'YetAnotherDatatypeProperty', YourOntology, 0, 1
)

class YetAnotherConcept(BaseClass):
    rdfs_isDefinedBy = YourOntology
    yetAnotherDatatypeProperty: YetAnotherDatatypeProperty[int]

class MultipleInheritanceConcept(SubConcept, YetAnotherConcept):
    pass
```

> NOTE the use of multiple inheritance is a controversial topic. It is at the developer's discretion to decide whether or not to use this feature. In any case, [good engineering practice](https://douroucouli.wordpress.com/2019/05/10/ontotip-single-inheritance-principle-considered-dangerous/) should be followed.


#### Custom member functions and class methods

One of the benefits of using OGM is that it is easy to relate the data processing logic on the Python side to the data in the Knowledge Graph. To achieve this, one can define custom functions:

```python
class YourConcept(BaseClass):
    rdfs_isDefinedBy = YourOntology

    # Member functions
    def your_custom_function(self):
        print('This is a custom function.')

    # Class methods
    @classmethod
    def your_custom_classmethod(cls):
        print(f'This is a custom classmethod of class {cls}.')
```


### Export Pydantic classes to triples

Once the developer is satisfied with the class definitions in Python, there are three ways to export it to the OWL format:

- Option 1: Export to a `rdflib.Graph` object
```python
g = YourOntology.export_to_graph()
```

- Option 2: Export to a file
```python
YourOntology.export_to_owl('your_ontology.ttl', format='turtle')
```

- Option 3: Export (upload) to a triple store
```python
from twa.kg_operations import PySparqlClient
sparql_endpoint = 'http://localhost:9999/blazegraph/namespace/kb/sparql'
sparql_client = PySparqlClient(sparql_endpoint, sparql_endpoint)
YourOntology.export_to_triple_store(sparql_client)
```

> See [Instantiation of the `PySparqlClient`](sparql.md/#instantiation-of-the-pysparqlclient) for more details on how to instantiate `PySparqlClient`.


## ABox level

### Instantiate an object in Python

Taking the classes `AnotherConcept` and `OneConcept` as an example:

```python
another_concept = AnotherConcept(anotherDatatypeProperty=3)

one_concept = OneConcept(
    oneDatatypeProperty='this is a data property',
    pointsToAnotherConcept=another_concept
)
```

The IRI of the instantiated instance can be accessed via `one_concept.instance_iri`, e.g. `https://www.theworldavatar.com/kg/yourontology/OneConcept_6481d535-160b-43f9-811e-80924daaabe7`

### Push new object to triple store

Assuming a sparql client is already instantiated, one can push the generated triples to knowledge graph:

```python
g_to_remove, g_to_add = one_concept.push_to_kg(sparql_client, recursive_depth=-1)
```

The above call collects all triples related to the objects `one_concept` and `another_concept`. This behaviour can be controlled via the `recursive_depth` flag.

> See [Instantiation of the `PySparqlClient`](sparql.md/#instantiation-of-the-pysparqlclient) for more details on how to instantiate `PySparqlClient`.


### Pull from triple store to create objects

For instances stored in the knowledge graph, one can pull it to the Python object with its IRI and the sparql client that is connected to the correct sparql endpoint:

```python
another_object_of_one_concept = OneConcept.pull_from_kg(
    'https://iri-of-the-object-of-interest',
    sparql_client,
    recursive_depth=-1
)
```

> NOTE the pulled objects will be stored in a list.

> NOTE the developer should be aware of the `recursive_depth` that one is using to pull the triples from the knowledge graph.

### Update existing objects in triple store

To make changes to the local objects and update it in the triple store:

```python
# Examples changes:
# Adding a new data property
one_concept.oneDatatypeProperty.add('this is a new data property')
# Removing the object property
one_concept.pointsToAnotherConcept.remove(another_concept)

# Push the changes to the triple store
one_concept.push_to_kg(sparql_client, recursive_depth=-1)
```

> NOTE the range of both object/data property are stored as `set`, which can be processed using built-in set operations.

> NOTE make sure the `recursive_depth` is specified correctly that all intended changes are pushed to the knowledge graph.

### Revert local changes

Conflicts can arise between local changes and remote updates when the knowledge graph is modified by multiple agents. This is a possible scenario given the distributed nature of our dynamic knowledge graph approach. An analogous situation is code conflicts in a Git repository when multiple people made changes to the same file. To address this, we provide a convenient function for developers to revert local changes:

```python
one_concept.revert_local_changes()
```

## Notes for future development

- How to generate Python script given an OWL file
- Add support for many-to-many cardinality constraints?
