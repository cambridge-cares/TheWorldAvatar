# Developing an Ontology Based Data Access (OBDA) project.

This instruction explains how to develop an OBDA project using Java programming language. The OBDA stands for accessing data stored in databases by using SPARQL and domain ontology. 

### Prerequisites

To implement this OBDA project, we are using a virtual knowledge graph system named as [Ontop](https://ontop-vkg.org), Postgresql jdbc driver, and H2 database jdbc driver. 
First step is to create empty Maven project in Eclipse. Aafter that we should add the following libraries into pom.xml file of Maven project.
 
* [Ontop Quest OWLAPI](https://mvnrepository.com/artifact/it.unibz.inf.ontop/ontop-quest-owlapi)
* [OpenRDF Sesame: Query Result IO SPARQL/JSON](https://mvnrepository.com/artifact/org.openrdf.sesame/sesame-queryresultio-sparqljson)
* [PostgreSQL JDBC Driver](https://mvnrepository.com/artifact/org.postgresql/postgresql) 
* [H2 JDBC Driver](https://rometools.github.io/rome/)
* [Ontop Quest Sesame](https://mvnrepository.com/artifact/it.unibz.inf.ontop/ontop-quest-sesame)
* [Logback Classic Module](https://mvnrepository.com/artifact/ch.qos.logback/logback-classic)
* [Logback Core Module](https://mvnrepository.com/artifact/ch.qos.logback/logback-core)

The latest version of Ontop libraries is available at the following Maven repository:

* [it.unibz.inf.ontop](https://mvnrepository.com/artifact/it.unibz.inf.ontop/ontop-obda-core)

### Installing

To install all libraries we are using the following command *mvn clean install -DskipTests*

### Steps in implementation OBDA example:

* Implementation relational database in Posstgresql or H2. 
* Implementation domain ontology (classes, properties, axioms). Ontology should be expressed in [OWL2 QL](https://www.w3.org/TR/owl2-profiles/#OWL_2_QL) 
* Implementaion mapping between ontology and database schema. This should be saved in file with *.obda* extension.
* Implementation sparql queries.


### Authors

* **[Nenad Krdzavac](caresssd@hermes.cam.ac.uk)**
* **[Feroz Farazi](msff2@cam.ac.uk)**