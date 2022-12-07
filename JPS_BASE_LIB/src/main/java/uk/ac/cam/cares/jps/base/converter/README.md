# TBox Generator

TBox Generator is developed to represent classes and properties of a TBox provided in a CSV (Comma-Separated Values) file-based template using the Web Ontology Language (OWL). TBox Generator also allows to describe data and metadata of a TBox, e.g., IRI, version and comment.

## CSV file-based template
### Header Row

The header row consists of the following attributes or columns:

|Source, Type, Target, Relation, Domain, Range, Quantifier, Comment, Defined By, Label  |
|---------------------------------------------------------------------------------------|
1. Source: the ontological element that needs to be defined.
2. Type: the type of the element provided in the Source column.
3. Target: the element that is related to the element in the Source column.
4. Relation: the relationship between the element in the Source column and the element in the Target column.
5. Domain: the domain of an object property or a data type property.
6. Range: the range of an object property or a data type property.
7. Quantifier: the quantifier that imposes a restriction on an object property or data type property.
8. Comment: a natural language description of the element provided in the Source column.
9. Defined By: the IRI of the ontology that defines the element in the Source column.
10. Label: a natural language label or name of the Source column element.

### Value Rows

#### TBox Metadata

1. The value rows that MUST follow the header row in the template are the TBox IRI, version, comment and import rows.
2. Assume that you want to develop a TBox called OntoKin with the IRI http://www.theworldavatar.com/ontology/ontokin, version number 1 and the comment "OntoKin is an ontology developed for representing chemical kinetic reaction mechanisms" and by importing the OntoCAPE TBox that has the IRI http://theworldavatar.com/ontology/ontocape/OntoCAPE.owl, then fill out the template as follows.

|Source, Type, Target, Relation, Domain, Range, Quantifier, Comment, Defined By, Label  |
|---------------------------------------------------------------------------------------|
|OntoKin, TBox, http://www.theworldavatar.com/kg/ontokin, https://www.w3.org/2007/05/powder-s#hasIRI, , , , , ,                                                                                         |
|OntoKin, TBox, 1, http://www.w3.org/2002/07/owl#versionInfo, , , , , , |
|OntoKin, TBox, OntoKin is an ontology developed for representing chemical kinetic reaction mechanisms, http://www.w3.org/2000/01/rdf-schema#comment, , , , , , |
|OntoKin, TBox,http://theworldavatar.com/ontology/ontocape/OntoCAPE.owl, http://www.w3.org/2002/07/owl#imports, , , , , , |

3. To skip the import of a TBox, do not provide any IRI in the corresponding position.

#### Classes and Properties
