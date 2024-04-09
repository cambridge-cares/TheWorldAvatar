`twa` package provides an implementation of Object Graph Mapper (OGM) using [Pydantic](https://github.com/pydantic/pydantic) to model the objects in Python memory which provides type validation, as well as [rdflib](https://github.com/RDFLib/rdflib) to host the objects in their triple format which can then be connected to a triple store using `twa.PySparqlClient`.

Below we provide minimal working example of how to use the OGM.

## TBox level


### Define an ontology (in Pydantic)

To begin with, you can define the ontology that hosts all concepts and relationships as below:

```python
from __future__ import annotations
from twa import *
from twa.data_model.base_ontology import as_range_of_object_property, as_range_of_data_property
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


### Define a property (relationship)

The two base classes `ObjectProperty` and `DataProperty` should be used to define custom object and data properties respectively. It should be noted that the user is only required to specify the `range` of these properties, as their `domain` will be automatically handled by the class that utilises the defined properties.


#### Object property

To define a custom object property:

```python
class PointsToAnotherConcept(ObjectProperty):
    # The user MUST provide the ontology for which the concept `is_defined_by_ontology`
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    # 0 and None for field `range` indicates no cardinality restriction
    range: as_range_of_object_property(AnotherConcept, 0, None)
```

which is equivalent to the below triples in OWL:

```turtle
<https://www.theworldavatar.com/kg/yourontology/PointsToAnotherConcept> a owl:ObjectProperty ;
    rdfs:domain <https://www.theworldavatar.com/kg/yourontology/OneConcept> ;
    rdfs:isDefinedBy <https://www.theworldavatar.com/kg/yourontology> ;
    rdfs:range <https://www.theworldavatar.com/kg/yourontology/AnotherConcept> .
```

> NOTE that the statement about `rdfs:domain` will be automatically added when defining concept `OneConcept`.


#### Data property

To define a custom data property:

```python
class OneDataProperty(DataProperty):
    # Same as ObjectProperty, `is_defined_by_ontology` is a compulsory field
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    range: as_range_of_data_property(str, 0, None)

class AnotherDataProperty(DataProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    # The cardinality means maximum 1
    range: as_range_of_data_property(int, 0, 1)
```

which is equivalent to the below triples in OWL:

```turtle
<https://www.theworldavatar.com/kg/yourontology/OneDataProperty> a owl:DatatypeProperty ;
    rdfs:domain <https://www.theworldavatar.com/kg/yourontology/OneConcept> ;
    rdfs:isDefinedBy <https://www.theworldavatar.com/kg/yourontology> ;
    rdfs:range xsd:string .

<https://www.theworldavatar.com/kg/yourontology/AnotherDataProperty> a owl:DatatypeProperty ;
    rdfs:domain <https://www.theworldavatar.com/kg/yourontology/AnotherConcept> ;
    rdfs:isDefinedBy <https://www.theworldavatar.com/kg/yourontology> ;
    rdfs:range xsd:integer .

<https://www.theworldavatar.com/kg/yourontology/AnotherConcept> rdfs:subClassOf [
    a owl:Restriction ;
    owl:maxQualifiedCardinality "1"^^xsd:nonNegativeInteger ;
    owl:onClass xsd:integer ;
    owl:onProperty <https://www.theworldavatar.com/kg/yourontology/AnotherDataProperty> ] .
```

> NOTE that the cardinality for `AnotherDataProperty` will be added as a [Blank Node](https://www.w3.org/TR/turtle/#BNodes) of `owl:Restriction` automatically to `AnotherConcept` when the class is defined.


### Define a class (concept)
```python
class AnotherConcept(BaseClass):
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    anotherDataProperty: AnotherDataProperty = Field(default_factory=AnotherDataProperty)

class OneConcept(BaseClass):
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    oneDataProperty: OneDataProperty = Field(default_factory=OneDataProperty)
    pointsToAnotherConcept: PointsToAnotherConcept = Field(default_factory=PointsToAnotherConcept)
```


#### Class and subclass
```python
class AdditionalDataProperty(DataProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    range: as_range_of_data_property(int, 0, 1)

class SubConcept(OneConcept):
    # As it inherits `OneConcept`, only additional object/data properties are required
    additionalDataProperty: AdditionalDataProperty = Field(default_factory=AdditionalDataProperty)
```


#### Multiple inheritance
```python
class YetAnotherDataProperty(DataProperty):
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    range: as_range_of_data_property(int, 0, 1)

class YetAnotherConcept(BaseClass):
    is_defined_by_ontology: ClassVar[BaseOntology] = YourOntology
    yetAnotherDataProperty: YetAnotherDataProperty = Field(default_factory=YetAnotherDataProperty)

class MultipleInheritanceConcept(SubConcept, YetAnotherConcept):
    pass
```


### Export Pydantic classes to triples
```python
# Export to rdflib.Graph object
g = YourOntology.export_to_graph()

# Export to file
YourOntology.export_to_owl('your_ontology.ttl', format='turtle')

# Export to triple store
from twa.kg_operations import PySparqlClient
sparql_endpoint = 'http://localhost:9999/blazegraph/namespace/kb/sparql'
sparql_client = PySparqlClient(sparql_endpoint, sparql_endpoint)
YourOntology.export_to_triple_store(sparql_client)
```

## ABox level

### Instantiate an object in Python

### Push new object to triple store

### Pull from triple store to create objects

### Update existing objects in triple store

## Notes for future development

1. How to generate Python script given an OWL file
2. Simplify the design of explicitly writing `dataProperty: DataProperty = Field(default_factory=DataProperty)`

TODO 2. Type[BaseOntology]? 3. as_range_of_data_property/as_range_of_object_property 4. transtive property
