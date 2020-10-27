# Developing an Ontology Based Data Access (OBDA) Project
### Authors
* [Nenad Krdzavac](caresssd@hermes.cam.ac.uk)
* [Feroz Farazi](msff2@cam.ac.uk)

OBDA is a means to access and query data stored in databases using SPARQL. This short document aims to describe the steps required to develop an OBDA project using Java and PostgreSQL relational database management system.

### Create a Maven project and add Dependencies

In the development of this OBDA project, you will employ [Ontop](https://ontop-vkg.org), which is a virtual knowledge-graph system. In addition, you will use OpenRDF, PostgreSQL, and Logback. Create a Maven project and include the following dependencies in the pom.xml file to enable the use of different OBDA related features supported by these software.
 
        <dependency>
            <groupId>it.unibz.inf.ontop</groupId>
            <artifactId>ontop-quest-owlapi</artifactId>
            <version>1.18.0</version>
        </dependency>
        <dependency>
            <groupId>it.unibz.inf.ontop</groupId>
            <artifactId>ontop-quest-sesame</artifactId>
            <version>1.18.0</version>
        </dependency>
        <dependency>
            <groupId>ch.qos.logback</groupId>
            <artifactId>logback-classic</artifactId>
            <version>1.0.2</version>
        </dependency>
        <dependency>
            <groupId>ch.qos.logback</groupId>
            <artifactId>logback-core</artifactId>
            <version>1.0.2</version>
        </dependency>
        <dependency>
            <groupId>org.openrdf.sesame</groupId>
            <artifactId>sesame-queryresultio-sparqljson</artifactId>
            <version>2.7.11</version>
        </dependency>
		  <dependency>
			   <groupId>org.postgresql</groupId>
			   <artifactId>postgresql</artifactId>
			   <version>42.2.14</version>
		  </dependency>

The dependencies provided above are collected from the following Maven repositories.

* [Ontop Quest OWLAPI](https://mvnrepository.com/artifact/it.unibz.inf.ontop/ontop-quest-owlapi)
* [OpenRDF Sesame: Query Result IO SPARQL/JSON](https://mvnrepository.com/artifact/org.openrdf.sesame/sesame-queryresultio-sparqljson)
* [PostgreSQL JDBC Driver](https://mvnrepository.com/artifact/org.postgresql/postgresql) 
* [Ontop Quest Sesame](https://mvnrepository.com/artifact/it.unibz.inf.ontop/ontop-quest-sesame)
* [Logback Classic Module](https://mvnrepository.com/artifact/ch.qos.logback/logback-classic)
* [Logback Core Module](https://mvnrepository.com/artifact/ch.qos.logback/logback-core)

### Create a database in PostgreSQL

If you do not have PostgreSQL installed on your computer, download and install it from the following link: [download](https://www.postgresql.org/download/). While setting up PostgreSQL, remember the *user name* and *password*.

* Create a relational database called *books* in PostgreSQL, and create a table called *tb_books* in this database. Add the following columns to this table: *bk_code*, *bk_title* and *bk_type*.


* Insert the following sample data, which are below the dashed line, into the *tb_books* table.

	bk_code, bk_title, bk_type
	-------------------------------
	'1', 'Java Programming', 'A'
	'2', 'Python Programming', 'B'
	'3', 'C++ Programming', 'A'
	'4', 'COBOL Programming', 'C'

For adding these data to the *tb_books* table, you can use the following INSERT Script:

	INSERT INTO public.tb_books(
		bk_code, bk_title, bk_type)
		VALUES ('1', 'Java Programming', 'A');
	INSERT INTO public.tb_books(
		bk_code, bk_title, bk_type)
		VALUES ('2', 'Python Programming', 'B');
	INSERT INTO public.tb_books(
		bk_code, bk_title, bk_type)
		VALUES ('3', 'C++ Programming', 'A');
	INSERT INTO public.tb_books(
		bk_code, bk_title, bk_type)
		VALUES ('4', 'COBOL Programming', 'C');

### Create or copy the exampleBooks.owl ontology

You can either copy the *exampleBooks.owl* ontology from [here](https://www.dropbox.com/home/IRP3%20CAPRICORN%20shared%20folder/_JPS%20Development/data) to the *resources* (src/main/resources) folder in the Maven project you created or develop the same ontology by following the steps below and put this under the same folder. If you already have copied the ontology into the resourced folder, you can go to the next section.

* Create an ontology with the OntologyIRI *http://theworldavatar.com/ontology/obda/exampleBooks.owl*. Include the classes from the following hierarchy in this ontology. Classes which have the same indentation are siblings, and classes which have different indentations are connected with subclass of relations. Classes indented to the right are subclasses of the class which is indented to the left and above. For example, Author and Book are siblings, AudioBook and E-Book are siblings, AudioBook is a subclass of Book and E-Book is a subclass of Book.

    Author
        EmergingWriter
    Book
        AudioBook
        E-Book
        IneditedBook
        PrintedBook
    Edition
        EconomicEdition
        SpecialEdition
    Edior

* Add the following object properties with the domain and range as indicated:
  

    name                     domain        range
    -------------------------------------------------
    editedBy                 Edition       Editor
    hasEdition               Book          Edition
    writtenBy                Book          Author
    
* Add the following data properties with the domain and range as indicated:


    name                     domain        range
    -------------------------------------------------
    dateOfFirstPublication   Edition       xsd:dateTime
    dateOfPublication        Edition       xsd:dateTime
    editionNumber            Edition       xsd:integer
    genre                    Book          xsd:string
    name                                   xsd:string
    title                    Book          xsd:string
    
It is important to remember that the expressivity of the ontology used in OBDA should not exceed the capability of [OWL2 QL](https://www.w3.org/TR/owl2-profiles/#OWL_2_QL). If your ontology contains only simple class hierarchies, object properties and data properties as above, it is OWL2 QL compatiable.

### Create a SPARQL query file

The following SPARQL query extracts the code and title of books. Save this query in a file called *book_code_title.rq* and put this file under the resources folder of the Maven project.

	PREFIX books: <http://theworldavatar.com/ontology/obda/exampleBooks.owl#>
	SELECT DISTINCT ?book ?title
	WHERE { 
	?book a books:Book .  
	?book books:title ?title .
	}


### Create a mapping file

Create a mapping file called *books_all.obda* in the resources folder of the Maven project and put the following three blocks of code into this file by maintaining the order of their appearance. This file establishes mapping(s) between a SPARQL query and the database via the ontology.

	[PrefixDeclaration]
	:   		http://theworldavatar.com/ontology/obda/exampleBooks.owl#
	owl:		http://www.w3.org/2002/07/owl#
	rdf:		http://www.w3.org/1999/02/22-rdf-syntax-ns#
	rdfs:		http://www.w3.org/2000/01/rdf-schema#
	
	[SourceDeclaration]
	sourceUri	Books
	connectionUrl	jdbc:postgresql://localhost/books
	username	postgres
	password    provide_your_postgresql_password
	driverClass	org.postgresql.Driver
	
	[MappingDeclaration] @collection [[
	
	mappingId	cl_Books
	target		:book/{bk_code}/ a :Book .
	source		select "bk_code" from "tb_books"
	
	mappingId	cl_title_Books
	target		:book/{bk_code}/ a :Book ; :title {bk_title} .
	source		select "bk_code", "bk_title" from "tb_books"
	
	]]

Do not forget to provide your user name and password for PostgreSQL in the SourceDeclaration block. If your user name is not postgres, provide the correct one.

The mapping file contains three blocks.

* It can be understood from the name *PrefixDeclaration* that the *first block* includes prefix declarations including *owl*, *rdf*, and *:*, which represents the prefix of the default namespace (the Ontology IRI followed by #) and in this particular example project it is *http://theworldavatar.com/ontology/obda/exampleBooks.owl#*.


* The *second block* is called *SourceDeclaration*, which includes information about *sourceUri*	that is the name of database. The feature *connectionUrl* represents  the Java Database Connectivity (*JDBC*) for the target database. *username* and *password*  are credentials for accessing the database. The *driverClass* is the driver class for the database. 

      
* The *third block* of the mapping is called *MappingDeclaration*, which can contain any number of mappings. In this example, it contains two mappings. A mapping consists of three elements: *mapping ID*, *target* and *source*. The *source* element specifies the SQL query that will be executed to query data from the database against a SPARQL query. The *target* element specifies how the data queried data are expressed as the output of the SPARQL query. This specification uses classes and properties from the ontology, variables from the SPARQL query, and the table name and column name of the database.

### Run Java code

Create a Java class with the name SPARQLPostgreSQLBookQuery under the package uk.ac.ceb.como.obda.postgresql, remove all content from the class file, copy and paste the following code into this empty file, and run this file as a Java Application.

	package uk.ac.ceb.como.obda.postgresql;
	import it.unibz.inf.ontop.sesame.RepositoryConnection;
	
	import it.unibz.inf.ontop.sesame.SesameVirtualRepo;
	
	import org.openrdf.query.Query;
	import org.openrdf.query.QueryLanguage;
	import org.openrdf.query.TupleQuery;
	import org.openrdf.query.TupleQueryResultHandler;
	import org.openrdf.query.resultio.text.csv.SPARQLResultsCSVWriter;
	
	import java.io.BufferedReader;
	import java.io.File;
	import java.io.FileOutputStream;
	import java.io.FileReader;
	import java.io.IOException;
	
	/**
	 * This class demonstrate how to perform SPARQL query on the books database<br>
	 * via the Book ontology using the Ontop, a virtual knowledge-graph system.
	 * 
	 * Outputs of the project are stored in a file called output.txt, which is<br>
	 * saved under user directory (the root directory of the Java project).
	 *
	 * @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
	 * @author Feroz Farazi (msff2@cam.ac.uk)
	 *
	 */
	
	public class SPARQLPostgreSQLBookQuery {

    /**
     * File containing the exampleBooks ontology.
     */
    final String owlFile = getClass().getClassLoader().getResource("exampleBooks.owl").getFile();

    /**
     * SPARQL query performed on the PostgreSQL book database via the exampleBooks ontology.
     */
    final String sparqlFile = getClass().getClassLoader().getResource("book_code_title.rq").getFile();
    
    /**
     * File containing mapping between the SPARQL query and books database<br> 
     * stored in PostgreSQL via exampleBooks ontology.
     */
    final String obdaFile = getClass().getClassLoader().getResource("books_all.obda").getFile();

    /**
     * File containing the output of the SPARQL query.
     */
    final String outputFile = System.getProperty("user.dir").concat(File.separator).concat("output.txt");

    private BufferedReader br;

    public static void main(String[] args) {

        try {

        	SPARQLPostgreSQLBookQuery example = new SPARQLPostgreSQLBookQuery();

            example.runSPARQL();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Performs SPARQL query on Book database via exampleBooks ontology and returns as
     * result book id and its title.
     */
    public void runSPARQL() throws Exception {

        String queryString = readSPARQL(sparqlFile);

        /**
         * Create and initialize repository
         * 
         */
        boolean existential = false;

        /**
         * 
         * A type of rewriting technique.
         * 
         * Reference paper that explains tree-withness rewriting technique is
         * 
         * "R. Kontchakov, M. Rodriguez-Muro and M. Zakharyaschev. Ontology-Based Data
         * Access with Databases: A Short Course. In S. Rudolph, G. Gottlob, I.
         * Horrocks, F. van Harmelen, editors, Summer School on Reasoning Web (Mannheim,
         * 30 July - 2 August), vol. 8067 of LNCS, pp. 194-229. Springer, 2013."
         * 
         */
        String rewriting = "TreeWitness";

        @SuppressWarnings("resource")
        SesameVirtualRepo repo = new SesameVirtualRepo("test_repo", owlFile, obdaFile, existential, rewriting);

        repo.initialize();

        try (RepositoryConnection conn = repo.getConnection()) {

            /**
             * Execute SPARQL query
             */
            Query query = conn.prepareQuery(QueryLanguage.SPARQL, queryString);

            TupleQuery tq = (TupleQuery) query;

            FileOutputStream resultingFile = new FileOutputStream(
                    new File(outputFile));

            TupleQueryResultHandler writer = new SPARQLResultsCSVWriter(resultingFile);

            /**
             * Stores SPARQL results into target txt file.
             */

            tq.evaluate(writer);
        }

        repo.shutDown();

    }

    /**
     * @author NK510 (caresssd@hermes.cam.ac.uk)
     * 
     * @param sparqlFile the sparql file path
     * @return the string that contains sparql query content
     * @throws IOException
     */
    private String readSPARQL(String sparqlFile) throws IOException {

        String queryString = "";

        br = new BufferedReader(new FileReader(sparqlFile));

        String line;

        while ((line = br.readLine()) != null) {
            queryString += line + "\n";
        }
        br.close();
        return queryString;
    }   
    }