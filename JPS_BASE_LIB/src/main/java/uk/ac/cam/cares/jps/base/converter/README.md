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

#### TBox Data and Metadata

1. The value rows that MUST follow the header row in the template are the TBox IRI, version, comment and import rows.
2. Assume that you want to develop a TBox called OntoKin with the IRI http://www.theworldavatar.com/ontology/ontokin, version number 1 and the comment "OntoKin is an ontology developed for representing chemical kinetic reaction mechanisms" and by importing the OntoCAPE TBox that has the IRI http://theworldavatar.com/ontology/ontocape/OntoCAPE.owl, then fill out the template as follows.

|Source, Type, Target, Relation, Domain, Range, Quantifier, Comment, Defined By, Label  |
|---------------------------------------------------------------------------------------|
|OntoKin, TBox, http://www.theworldavatar.com/kg/ontokin, https://www.w3.org/2007/05/powder-s#hasIRI, , , , , ,                                                                                         |
|OntoKin, TBox, 1, http://www.w3.org/2002/07/owl#versionInfo, , , , , , |
|OntoKin, TBox, OntoKin is an ontology developed for representing chemical kinetic reaction mechanisms, http://www.w3.org/2000/01/rdf-schema#comment, , , , , , |
|OntoKin, TBox,http://theworldavatar.com/ontology/ontocape/OntoCAPE.owl, http://www.w3.org/2002/07/owl#imports, , , , , , |

> **_NOTE:_** To skip the import of a TBox, do not provide any IRI in the corresponding position.

#### Classes

Classes MUST follow the TBox Data and Metadata block.
Assume that you want to:
1. Define the classes Reaction Mechanism, Phase, Bulk Phase, and Rate Coefficient with the following descriptions:
     - Reaction Mechanism: A reaction mechanism refers to a set of elementary reactions with specific rate laws, for example to model the combustion of hydrogen.
     - Phase: A phase of a substance is a form of matter.
     - Bulk Phase: A solid phase that is contiguous with site phases and remote from the gas phase.
     - Rate Coefficient: The coefficients used to evaluate the reaction rate expression.
 2. Desribe that the Bulk Phase class is a subclass of the Phase class and the Rate Coefficient class is equivalent to the Reaction Rate Coefficient class defined in the Cyber Physical System (CPS) Behaviour ontology available at http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl.
 3. Specify that theses classes are defined in an ontology that has the following URL: http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl

To achieve these goals, fill out the template as follows.

|Source, Type, Target, Relation, Domain, Range, Quantifier, Comment, Defined By, Label  |
|---------------------------------------------------------------------------------------|
|ReactionMechanism, Class, , , , , , "A reaction mechanism refers to a set of elementary reactions with specific rate laws, for example to model the combustion of hydrogen.", http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Reaction Mechanism |
|Phase, Class, , , , , , A phase of a substance is a form of matter., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Phase |
|BulkPhase, Class, Phase, IS-A, , , , A solid phase that is contiguous with site phases and remote from the gas phase., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Bulk Phase |
|RateCoefficient, Class, http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ReactionRateCoefficient, EQUIVALENT-TO, , , , The coefficients used to evaluate the reaction rate expression., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Rate Coefficient |

> **_NOTE:_**  A) Write class names using CamelBack notation as shown under the Source column in the table above. Some examples are ReactionMechanism, Phase, and BulkPhase. B) Provide the user facing name of the class under the Label column. Some examples are Reaction Mechanism, Phase and Bulk Phase. C) Define ontological subclass of relationship using IS-A and equivalent class relationship using EQUIVALENT-TO. D) Enclose any description containing a comma provided under the Comment column within double quote. For example, see the comment of the ReactionMechansim class. E) Currently, the tool does not support the representation of the disjoint class relationship. F) Provide the complete URL of classes reused from another ontology. For example, ReactionRateCoefficient is reused from the Cyber Physical System Behaviour ontology, therefore, its URL http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ReactionRateCoefficient is provided.

#### Object Properties
Object properties can be represented just below [TBox Data and Metadata](####TBox-Data-and-Metadata) rows. However, it is recommended that object properties should be provided after classes.

Assume that you want to:
1. define the object properties exists in, contained in and has element with the following descriptions:
- exists in: A relation between a site phase or bulk phase and a material in which they exist.
- contained in: A relation that identifies that a gas phase or material is contained in a reaction mechanism.
- has element: A relation that defines that a species or molecular entity contains a chemical element.
2. specify that a Site Phase or Bulk Phase exists in a Material, a Gas Phase or Material is contained in a Reaction Mechanism and a Species has an Element.
3. impose a restriction on the exists in relation to express that when this relation is applied to any instance of Site Phase or Bulk Phase it can only be linked to an instance of Material.
4. impose a cardinality restriction on the contained in relation to express that any instance of Gas Phase or Material must be linked to exactly 1 instance of Reaction Mechanism.

To achieve these goals, fill out the template as follows:

|Source, Type, Target, Relation, Domain, Range, Quantifier, Comment, Defined By, Label  |
|---------------------------------------------------------------------------------------|
|existsIn,Object Property,,,Site Phase UNION Bulk Phase,Material,only,A relation between a site phase or bulk phase and a material in which they exist.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, |
|containedIn,Object Property,,,Gas Phase UNION Material,Reaction Mechanism,exactly 1,A relation that identifies that a gas phase or material is contained in a reaction mechanism.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, |
|hasElement,Object Property,,,Species,Element,,A relation that defines that a species or molecular entity contains a chemical element.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, |

> **_NOTE:_**  A) Object property names have the similar syntax of CamelBack notation except the beginning lower case letter. For example, existsIn and containedIn. B) An example cardinality restriction is shown in the definition of the containedIn object property. To represent the cardinality of at least 1 provide minimum 1 or and for at most 1 provide maxium 1. Currently, the TBox Generator does not support the cardinality more than 1, for example, exactly 2, at least 3 or at most 4 are not supported.

#### Data Properties
Data properties or Datatype properties can be represented just below [TBox Data and Metadata](####TBox-Data-and-Metadata) rows. However, it is recommended that data properties should be provided after classes. Data properties can be provided before or after object properties.

Assume that you want to:
1. define the data properties identifier, dimension and requires species validation.
2. specify that the data type of identifier is String
3. specify that the data type of dimension is Integer and it is applied to the instances of the Phase class
4. specify that the data type of requires species validation is String and it is applied to the instances of the Reaction Mechanism class.

To achieve these goals, fill out the template as follows:

|Source, Type, Target, Relation, Domain, Range, Quantifier, Comment, Defined By, Label  |
|---------------------------------------------------------------------------------------|
|http://purl.org/dc/elements/1.1/identifier,Data Property,,,,String,,,http://purl.org/dc/elements, |
|http://www.opengis.net/ont/geosparql#dimension,Data Property,,,Phase,Integer,,,http://www.opengis.net/ont/geosparql, |
|requiresSpeciesValidation,Data Property,,,Reaction Mechanism,String,,,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, |

> **_NOTE:_**  A) Data property names have the similar syntax of CamelBack notation except the beginning lower case letter. For example, requiresSpeciesValidation.