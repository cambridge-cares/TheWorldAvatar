`twa` package provides an implementation of Object Graph Mapper (OGM) using [Pydantic](https://github.com/pydantic/pydantic) to model the objects in Python memory which provides type validation, as well as [rdflib](https://github.com/RDFLib/rdflib) to host the objects in their triple format which can then be connected to a triple store using `twa.PySparqlClient`.

Below we provide minimal working example of how to use the OGM.

## TBox level


### Define an ontology (in Pydantic)

To begin with, you can define the ontology that hosts all concepts and relationships as below:

```python
# Import relevant packages
from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty, as_range
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

To define custom object and data properties, the two base classes `ObjectProperty` and `DatatypeProperty` should be used respectively. It should be noted that the user is only required to specify the `range` of these properties, as their `domain` will be automatically handled by the class that utilises the defined properties.


#### Object property

To define a custom object property:

```python
class PointsToAnotherConcept(ObjectProperty):
    # The user MUST provide the ontology for which the concept `is_defined_by_ontology`
    # Since `is_defined_by_ontology` is already defined as a ClassVar, the user can just assign a value here
    is_defined_by_ontology = YourOntology
    # 0 and None for field `range` indicates no cardinality restriction (which is also the default value)
    # Therefore, the below line is equivalent to `range: as_range(AnotherConcept)`
    range: as_range(AnotherConcept, 0, None)
```

which is equivalent to the below triples in OWL:

```turtle
yo:PointsToAnotherConcept a owl:ObjectProperty ;
    rdfs:domain yo:OneConcept ;
    rdfs:isDefinedBy <https://www.theworldavatar.com/kg/yourontology> ;
    rdfs:range yo:AnotherConcept .
```

> NOTE that the statement about `rdfs:domain` will be automatically added when defining concept that uses this object property, e.g. assume `OneConcept` uses this object property: ```yo:PointsToAnotherConcept rdfs:domain yo:OneConcept .```
>
> NOTE for multiple concepts as domain of the same object property, e.g. assume both `OneConcept` and `SubConcept` are the domain, then a [Blank Node](https://www.w3.org/TR/turtle/#BNodes) of `owl:Class` will be added: ```yo:pointsToAnotherConcept rdfs:domain [ a owl:Class ; owl:unionOf ( yo:OneConcept yo:SubConcept ) ] ;```


##### Transitive property
Transitive property is a specific type of object property, it can be very useful for representing [part-whole relations](https://www.w3.org/2001/sw/BestPractices/OEP/SimplePartWhole/). To define a custom transitive property:

```python
class OneTransitiveProperty(TransitiveProperty):
    is_defined_by_ontology = YourOntology
    range: as_range(OneClassWithTransitive)

# Here we also provide the class definition for the concept that makes use of `OneTransitiveProperty`
# Please refer to later part of this documentation for more examples on how to define a class
class OneClassWithTransitive(BaseClass):
    is_defined_by_ontology = YourOntology
    # Note that here the range and domain of `OneTransitiveProperty` are both `OneClassWithTransitive`
    oneTransitiveProperty: OneTransitiveProperty
```

A convenient member function is provided to retrieve all transitive objects as a [`set`](https://docs.python.org/3/tutorial/datastructures.html#sets):

```python
# Assume we have instantiated an object `one_transitive_property` of class `OneTransitiveProperty`
set_of_transitive_objects = one_transitive_property.obtain_transitive_objects()
```


#### Data property

To define a custom data property:

```python
class OneDatatypeProperty(DatatypeProperty):
    # Same as ObjectProperty, `is_defined_by_ontology` is a compulsory field
    is_defined_by_ontology = YourOntology
    range: as_range(str)

class AnotherDatatypeProperty(DatatypeProperty):
    is_defined_by_ontology = YourOntology
    # The cardinality means maximum 1
    range: as_range(int, 0, 1)
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

> NOTE that the cardinality for `AnotherDatatypeProperty` will be added as a [Blank Node](https://www.w3.org/TR/turtle/#BNodes) of `owl:Restriction` automatically to `AnotherConcept` when the class is defined.


### Define a class (concept)

The classes can be defined in the normal way as defining Python native classes, with the field being the object/data properties previously defined:

```python
class AnotherConcept(BaseClass):
    # Like object/data properties, `is_defined_by_ontology` is a compulsory field
    is_defined_by_ontology = YourOntology
    anotherDatatypeProperty: AnotherDatatypeProperty

class OneConcept(BaseClass):
    is_defined_by_ontology = YourOntology
    oneDatatypeProperty: OneDatatypeProperty
    pointsToAnotherConcept: PointsToAnotherConcept
```

> NOTE that the name of field **CAN NOT** be the same as the name of the corresponding Pydantic class it is referring to, i.e. `AnotherDatatypeProperty: AnotherDatatypeProperty` would be invalid.


#### Class and subclass

The subclass relationship `rdfs:subClassOf` can also be defined following the standard practice, e.g. we define a concept `SubConcept` `rdfs:subClassOf` `OneConcept`:

```python
class AdditionalDatatypeProperty(DatatypeProperty):
    is_defined_by_ontology = YourOntology
    range: as_range(int, 0, 1)

class SubConcept(OneConcept):
    # As it inherits `OneConcept`, only additional object/data properties are required
    additionalDatatypeProperty: AdditionalDatatypeProperty
```


#### Multiple inheritance

Multiple inheritance is also possible:

```python
class YetAnotherDatatypeProperty(DatatypeProperty):
    is_defined_by_ontology = YourOntology
    range: as_range(int, 0, 1)

class YetAnotherConcept(BaseClass):
    is_defined_by_ontology = YourOntology
    yetAnotherDatatypeProperty: YetAnotherDatatypeProperty

class MultipleInheritanceConcept(SubConcept, YetAnotherConcept):
    pass
```

> NOTE the use of multiple inheritance is a controversial topic. It is at the developer's discretion to decide whether or not to use this feature. In any case, [good engineering practice](https://douroucouli.wordpress.com/2019/05/10/ontotip-single-inheritance-principle-considered-dangerous/) should be followed.


#### Custom member functions and class methods

One of the benefits of using OGM is that it is easy to relate the data processing logic on the Python side to the data in the Knowledge Graph. To achieve this, one can define custom functions:

```python
class YourConcept(BaseClass):
    is_defined_by_ontology = YourOntology

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
one_concept.oneDatatypeProperty.range.add('this is a new data property')
# Removing the object property
one_concept.pointsToAnotherConcept.range.remove(another_concept)

# Push the changes to the triple store
one_concept.push_to_kg(sparql_client, recursive_depth=-1)
```

> NOTE both object/data property use `set` to store the range of the property.

> NOTE make sure the `recursive_depth` is specified correctly that all intended changes are pushed to the knowledge graph.

### Revert local changes

Conflicts can arise between local changes and remote updates when the knowledge graph is modified by multiple agents. This is a possible scenario given the distributed nature of our dynamic knowledge graph approach. An analogous situation is code conflicts in a Git repository when multiple people made changes to the same file. To address this, we provide a convenient function for developers to revert local changes:

```python
one_concept.revert_local_changes()
```

## Notes for future development

- How to generate Python script given an OWL file
