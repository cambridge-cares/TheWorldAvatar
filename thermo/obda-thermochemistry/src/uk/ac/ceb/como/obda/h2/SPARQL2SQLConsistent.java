package uk.ac.ceb.como.obda.h2;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import it.unibz.inf.ontop.model.OBDAModel;
import it.unibz.inf.ontop.owlrefplatform.owlapi.*;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntology;

import static java.util.stream.Collectors.joining;

/**
 * 
 * @author NK520 (caresssd@hermes.cam.ac.uk)
 * 
 * This class demonstrates how to convert sparql query into sql query.
 * 
 */
public class SPARQL2SQLConsistent {


	/**
	 * exampleBooks ontology file path. ddata are stored in database.
	 */
	final String owlFile = "./resources/books/exampleBooks.owl";
	/**
	 * Mapping between exampleBooks ontology and 'books' Postgresql database
	 */
	final String obdaFile = "./resources/books/bk_code.obda";
	/**
	 * Sparql query file path. Result of that query should be instances of ontology class
	 * Book.
	 */
	final String sparqlFile = "./resources/books/book_id.rq";

	public static void main(String[] args) {
		try {
			SPARQL2SQLConsistent example = new SPARQL2SQLConsistent();
			example.generateSQLFromSPARQL();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Generates SQL from given input SPARQL
	 * 
	 * @throws Exception
	 */
	public void generateSQLFromSPARQL() throws Exception {

		OWLOntology ontology = OWLManager.createOWLOntologyManager()
				.loadOntologyFromOntologyDocument(new File(owlFile));

		OBDAModel obdaModel = new MappingLoader().loadFromOBDAFile(obdaFile);

		QuestOWLFactory factory = new QuestOWLFactory();

		QuestOWLConfiguration reasonerConfig = QuestOWLConfiguration.builder().obdaModel(obdaModel).build();

		String sparqlQuery = Files.lines(Paths.get(sparqlFile)).collect(joining("\n"));

		/**
		 * Instantiate quest reasoner.
		 */
		QuestOWL reasoner = factory.createReasoner(ontology, reasonerConfig);

		System.out.println();

		QuestOWLConnection conn = reasoner.getConnection();

		QuestOWLStatement st = conn.createStatement();

		String sqlQuery = st.getUnfolding(sparqlQuery);

		try {
			BufferedWriter out = new BufferedWriter(new FileWriter("./resources/sparql_to_sql_h2.txt"));
			
			out.write("reasoner.isConsistent() : " + reasoner.isConsistent() + "\n");
			out.write("\n");
			out.write("\n");
			out.write("Input SPARQL Query \n");
			out.write(sparqlQuery);
			out.write("\n");
			out.write("\n");
			out.write("Output SQL Query \n");
			out.write("\n");
			out.write("\n");
			out.write(sqlQuery);
			out.close();
		} catch (IOException e) {
			System.out.println("Exception during rewriting (no method) sparql or sql to a file");

		}
	}
	
	
	
	
}