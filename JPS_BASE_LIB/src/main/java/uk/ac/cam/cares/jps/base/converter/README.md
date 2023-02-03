# TBox Generator

TBox Generator is developed to represent classes and properties of a TBox provided in a CSV (Comma-Separated Values) file-based template using the [Web Ontology Language (OWL)](#https://www.w3.org/TR/owl-ref/). TBox Generator requires to describe metadata of a TBox, e.g., IRI, version and comment. You can use a text editor such as Notepad++ or any spreadsheet software such as Google Sheets or Microsoft Excel to fill out the CSV-file-based template. This documentation shows how to fill out the template using a text editor.

## CSV file-based template
### Header Row

The header row consists of the following attributes or columns:

|Source,Type,Target,Relation,Domain,Range,Quantifier,Comment,Defined By,Label|
|----------------------------------------------------------------------------|
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
Values rows include TBox metadata, classes, object properties and data properties.
#### TBox Metadata
1. The value rows that MUST follow the header row in the template are the TBox IRI, version, comment and import rows.
2. Assume that you want to develop a TBox called OntoKin with the IRI http://www.theworldavatar.com/ontology/ontokin, version number 1 and the comment "OntoKin is an ontology developed for representing chemical kinetic reaction mechanisms" and by importing the OntoCAPE TBox that has the IRI http://theworldavatar.com/ontology/ontocape/OntoCAPE.owl, then fill out the template as follows.

|Source,Type,Target,Relation,Domain,Range,Quantifier,Comment,Defined By,Label|
|----------------------------------------------------------------------------|
|OntoKin, TBox, http://www.theworldavatar.com/kg/ontokin, https://www.w3.org/2007/05/powder-s#hasIRI, , , , , , |
|OntoKin,TBox,1,http://www.w3.org/2002/07/owl#versionInfo, , , , , , |
|OntoKin,TBox,OntoKin is an ontology developed for representing chemical kinetic reaction mechanisms,http://www.w3.org/2000/01/rdf-schema#comment, , , , , , |
|OntoKin,TBox,http://theworldavatar.com/ontology/ontocape/OntoCAPE.owl,http://www.w3.org/2002/07/owl#imports, , , , , , |

> **_NOTE:_** To skip the import of a TBox, do not provide any IRI in the corresponding position. To import multiple TBoxes, provide IRIs separated by a comma and enclose the IRIs within double quotes.

#### Classes

Classes MUST follow the TBox Data and Metadata block.
Assume that you want to:
1. Define classes Reaction Mechanism, Phase, Gas Phase, Site Phase, Bulk Phase, Species, Element, Material and Rate Coefficient with the following descriptions:
     - Reaction Mechanism: A reaction mechanism refers to a set of elementary reactions with specific rate laws, for example, to model the combustion of hydrogen.
     - Phase: A phase of a substance is a form of matter.
     - Gas Phase: A continuous gaseous phase.
     - Site Phase: A phase that exists at the interface between the gas phase and a bulk phase.
     - Bulk Phase: A solid phase that is contiguous with site phases and remote from the gas phase.
     - Species: An ensemble of chemically identical molecular entities (McNaught & Wilkinson, 1997).
     - Element: An atom or isotope.
     - Material: A substance that contains at least one site or bulk phase.
     - Rate Coefficient: The coefficients used to evaluate the reaction rate expression.
 2. Describe that the Gas Phase, Site Phase, and Bulk Phase classes are subclasses of the Phase class. The Rate Coefficient class is equivalent to the Reaction Rate Coefficient class defined in the Cyber-Physical System (CPS) Behaviour ontology available at http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl.
 3. Specify that these classes are defined in an ontology with the following URL: http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl.

To achieve these goals, fill out the template as follows.

|Source, Type, Target, Relation, Domain, Range, Quantifier, Comment, Defined By, Label  |
|---------------------------------------------------------------------------------------|
|ReactionMechanism,Class, , , , , ,A reaction mechanism refers to a set of elementary reactions., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Reaction Mechanism|
|Phase,Class, , , , , ,A phase of a substance is a form of matter., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Phase|
|GasPhase,Class,Phase,IS-A, , , ,A continuous gaseous phase.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Gas Phase|
|SitePhase,Class,Phase,IS-A, , , ,A phase that exists at the interface between the gas phase and a bulk phase.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Site Phase|
|BulkPhase,Class,Phase,IS-A, , , ,A solid phase that is contiguous with site phases and remote from the gas phase., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Bulk Phase|
|Species,Class, , , , , ,An ensemble of chemically identical molecular entities (McNaught & Wilkinson - 1997).,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Species|
|Element,Class, , , , , ,An atom or isotope.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Element|
|Material,Class, , , , , ,A substance that contains at least one site or bulk phase.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Material|
|RateCoefficient,Class,http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ReactionRateCoefficient,EQUIVALENT-TO, , , ,The coefficients used to evaluate the reaction rate expression.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Rate Coefficient |

> **_NOTE:_**  A) Class names have the CamelBack notation, as shown under the Source column in the table above. Some examples are ReactionMechanism, Phase, and GasPhase. B) User-facing names of classes are provided under the Label column. Some examples are Reaction Mechanism, Phase and Gas Phase. C) Defined ontological subclass of relationship using IS-A and equivalent class relationship using EQUIVALENT-TO. D) Any description containing a comma should be enclosed within double quotes. E) Provided the complete URL of classes reused from another ontology. For example, ReactionRateCoefficient is reused from the Cyber-Physical Systems Behaviour ontology; therefore, its URL http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ReactionRateCoefficient is used.

##### Class-class relationships
The [OWL](#https://www.w3.org/TR/owl-ref/) language-supported constructs rdfs:subClassOf and owl:equivalentClass are used for representing subclass of, and equivalent class relationships, respectively. The mapping between these relationships and the TBox Generator acceptable relationships is shown below:

|  OWL relationship                      | TBox Generator acceptable relationship |
| ---------------------------------------| -------------------------------------- |
| rdfs:subClassOf                        | IS-A                                   |
| owl:equivalentClass                    | EQUIVALENT-TO                          |

> **_NOTE:_** Currently, owl:disjointWith, which expresses the disjoint relationship between classes, is not supported.

#### Object Properties
Object properties can be represented just below TBox Metadata rows. However, it is recommended that object properties should be provided after classes.

Assume that you want to:
1. define the object properties exists in, contained in and has element with the following descriptions:
- exists in: A relation between a site phase or bulk phase and a material in which they exist.
- contained in: A relation that identifies that a gas phase or material is contained in a reaction mechanism.
- has element: A relation that defines that a species or molecular entity contains a chemical element.
2. specify that a Site Phase or Bulk Phase exists only in a Material, a Gas Phase or Material is contained exactly in one Reaction Mechanism and a Species has an Element.
3. impose a restriction on the exists in relation to express that when this relation is applied to any instance of Site Phase or Bulk Phase it can only be linked to an instance of Material.
4. impose a cardinality restriction on the contained in relation to express that any instance of Gas Phase or Material must be linked to exactly 1 instance of Reaction Mechanism.

To achieve these goals, fill out the template as follows:

|Source,Type,Target,Relation,Domain,Range,Quantifier,Comment,Defined By,Label|
|----------------------------------------------------------------------------|
|existsIn,Object Property, , ,SitePhase UNION BulkPhase,Material,only,A relation between a site phase or bulk phase and a material in which they exist.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, exists in|
|containedIn,Object Property, , ,GasPhase UNION Material,ReactionMechanism,exactly 1,A relation that identifies that a gas phase or material is contained in a reaction mechanism.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,contained in|
|hasElement,Object Property, , ,Species,Element, ,A relation that defines that a species or molecular entity contains a chemical element.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,has element|

> **_NOTE:_**  A) Object property names have a similar syntax to CamelBack notation except for the beginning lowercase letter. For example, existsIn and containedIn. B) To specify that a Site Phase or Bulk Phase exists only in a Material, SitePhase UNION BulkPhase is represented under the Domain column, Material is represented under the Range column and only is provided under the Quantifier column. C) An example cardinality restriction is shown in the definition of the containedIn object property. To represent the cardinality of at least 1, provide minimum 1 or and for at most 1, provide maxium 1. Currently, the TBox Generator does not support the cardinality more than 1. For example, exactly 2, at least 3 or at most 4 are not supported.

#### Data Properties
Data properties or datatype properties can be represented just below TBox Metadata rows. However, it is recommended that data properties are provided after classes. Data properties can be provided above or below object properties.

Assume that you want to:
1. define the data properties identifier, dimension and requires species validation.
2. specify that the data type of identifier is String
3. specify that the data type of dimension is Integer and it is applied to the instances of the Phase class
4. specify that the data type that requires species validation is String, and it is applied to the instances of the Reaction Mechanism class.

To achieve these goals, fill out the template as follows:

|Source,Type,Target,Relation,Domain,Range,Quantifier,Comment,Defined By,Label|
|----------------------------------------------------------------------------|
|http://purl.org/dc/elements/1.1/identifier,Data Property, , , ,String, , ,http://purl.org/dc/elements, |
|http://www.opengis.net/ont/geosparql#dimension,Data Property, , ,Phase,Integer, , ,http://www.opengis.net/ont/geosparql, |
|requiresSpeciesValidation,Data Property, , ,Reaction Mechanism,String, , ,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, |

> **_NOTE:_**  A) Data property names have a similar syntax to CamelBack notation except for the beginning lowercase letter. For example, requiresSpeciesValidation. B) Complete URLs are provided for the properties reused from external ontologies. For example, the URLs http://purl.org/dc/elements/1.1/identifier and http://www.opengis.net/ont/geosparql#dimension are used in the definitions of identifier and dimension, respoectively.

##### Data types
TBox Generator supports all [OWL2 data types](#http://owlcs.github.io/owlapi/apidocs_5/org/semanticweb/owlapi/vocab/OWL2Datatype.html) included in the OWL API. Data types in the CSV file template are case insensitive and do not require to be prefixed with XSD, RDF, RDFS or OWL. For example, instead of XSD_STRING, you can write String or string. The complete mapping between the OWL2 data type and the TBox Generator acceptable data type is shown below.

| OWL2 data type      | TBox Generator acceptable data type |
| ------------------- | ----------------------------------- |
| OWL_RATIONAL        | rational                            |
| OWL_REAL            | real                                |
| RDF_LANG_STRING     | lang string                         |
| RDF_PLAIN_LITERAL   | plain literal                       |
| RDF_XML_LITERAL     | xml literal                         |
| XSD_ANY_URI         | any uri                             |
| XSD_BASE_64_BINARY  | base 64 binary                      |
| XSD_BOOLEAN         | boolean                             |
| XSD_BYTE            | byte                                |
| XSD_DATE_TIME       | date time                           |
| XSD_DATE_TIME_STAMP | date time stamp                     |
| XSD_DECIMAL         | decimal                             |
| XSD_DOUBLE          | double                              |
| XSD_FLOAT           | float                               |
| XSD_HEX_BINARY      | hex binary                          |
| XSD_INT             | int                                 |
| XSD_INTEGER         | integer                             |
| XSD_LANGUAGE        | language                            |
| XSD_LONG            | long                                |
| XSD_NAME            | name                                |
| XSD_NCNAME          | ncname                              |
| XSD_NEGATIVE_INTEGER| negative integer                    |
| XSD_NMTOKEN         | nmtoken                             |
| XSD_NON_NEGATIVE_INTEGER| non negative integer            |
| XSD_NON_POSITIVE_INTEGER| non positive integer            |
| XSD_NORMALIZED_STRING| normalized string                  |
| XSD_POSITIVE_INTEGER| positive integer                    |
| XSD_SHORT           | short                               |
| XSD_STRING          | string                              |
| XSD_TOKEN           | token                               |
| XSD_UNSIGNED_BYTE   | unsigned byte                       |
| XSD_UNSIGNED_INT    | unsigned int                        |
| XSD_UNSIGNED_LONG   | unsigned long                       |
| XSD_UNSIGNED_SHORT  | unsigned short                      |

## Filled out CSV file-based template
Suppose you followed the instructions for filling out the template in the TBox Metadata, Classes, Object Properties and Data Properties sections. In that case, your CSV file will have precisely the following information.

|Source,Type,Target,Relation,Domain,Range,Quantifier,Comment,Defined By,Label|
|----------------------------------------------------------------------------|
|OntoKin, TBox, http://www.theworldavatar.com/kg/ontokin, https://www.w3.org/2007/05/powder-s#hasIRI, , , , , , |
|OntoKin,TBox,1,http://www.w3.org/2002/07/owl#versionInfo, , , , , , |
|OntoKin,TBox,OntoKin is an ontology developed for representing chemical kinetic reaction mechanisms,http://www.w3.org/2000/01/rdf-schema#comment, , , , , , |
|OntoKin,TBox,http://theworldavatar.com/ontology/ontocape/OntoCAPE.owl,http://www.w3.org/2002/07/owl#imports, , , , , , |
|ReactionMechanism,Class, , , , , ,A reaction mechanism refers to a set of elementary reactions., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Reaction Mechanism|
|Phase,Class, , , , , ,A phase of a substance is a form of matter., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Phase|
|GasPhase,Class,Phase,IS-A, , , ,A continuous gaseous phase.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Gas Phase|
|SitePhase,Class,Phase,IS-A, , , ,A phase that exists at the interface between the gas phase and a bulk phase.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, Site Phase|
|BulkPhase,Class,Phase,IS-A, , , ,A solid phase that is contiguous with site phases and remote from the gas phase., http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Bulk Phase|
|Species,Class, , , , , ,An ensemble of chemically identical molecular entities (McNaught & Wilkinson - 1997).,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Species|
|Element,Class, , , , , ,An atom or isotope.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Element|
|Material,Class, , , , , ,A substance that contains at least one site or bulk phase.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Material|
|RateCoefficient,Class,http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ReactionRateCoefficient,EQUIVALENT-TO, , , ,The coefficients used to evaluate the reaction rate expression.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,Rate Coefficient |
|existsIn,Object Property, , ,SitePhase UNION BulkPhase,Material,only,A relation between a site phase or bulk phase and a material in which they exist.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, exists in|
|containedIn,Object Property, , ,GasPhase UNION Material,ReactionMechanism,exactly 1,A relation that identifies that a gas phase or material is contained in a reaction mechanism.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,contained in|
|hasElement,Object Property, , ,Species,Element, ,A relation that defines that a species or molecular entity contains a chemical element.,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl,has element|
|http://purl.org/dc/elements/1.1/identifier,Data Property, , , ,String, , ,http://purl.org/dc/elements, |
|http://www.opengis.net/ont/geosparql#dimension,Data Property, , ,Phase,Integer, , ,http://www.opengis.net/ont/geosparql, |
|requiresSpeciesValidation,Data Property, , ,Reaction Mechanism,String, , ,http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl, |

## Conversion into OWL
Copy all tabular data from the *Filled out CSV file-based template* Section, paste them in a newly opened Notepad++ file and save the file as a CSV file.
> **_NOTE:_**  TBox Generator supports a specific type of CSV file. UTF-8 encoded CSV files created in Microsoft Excel don't work.

Once you have created the CSV file, you can convert it into OWL by running [TBoxGenerator](#https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/converter/TBoxGeneration.java) developed using Java.

To run TBox Generator from Python, use the following code:
```
from py4jps.resources import JpsBaseLib

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.converter.*")
tbox_generation = jpsBaseLib_view.TBoxGeneration()
# NOTE replace './ontosomething.csv' with the desired ontology
tbox_generation.generateTBox('./<FILE_NAME>.csv')
```
