package uk.ac.ceb.como.obda.postgresql;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Paths;

import it.unibz.inf.ontop.model.OBDAModel;
import it.unibz.inf.ontop.owlrefplatform.owlapi.*;

import org.semanticweb.owlapi.apibinding.OWLManager;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import static java.util.stream.Collectors.joining;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 *         This example demonstrates performing SPARQL query under reasoning
 *         mode. It demonstrates inconsistency of sparql result where instances
 *         of books:AudioBook and books:E-Book are disjoint.
 *
 *         This example demonstrates how to access data in 'books' Postgresql database via exampleBooks ontology.
 */
public class SPARQLPostgresqlInconsistency {

	/**
	 * The exampleBooks ontology file path. ddata are stored in database.
	 */
	final String owlFile = "./resources/books/exampleBooks.owl";
	/**
	 * Mapping between exampleBooks ontology and books database stored in Postgresql.
	 */
	final String obdaFile = "./resources/postgresql/bk_id_audio_book_postgresql_inconsistency.obda";
	
	/**
	 * SPARQL query performed on Postgresql book database via exampleBooks ontology.
	 */
	final String sparqlFile = "./resources/books/book_id_audio_and_ebook.rq";

	public static void main(String[] args) {
		try {
			SPARQLPostgresqlInconsistency sparqlInConsistentInference = new SPARQLPostgresqlInconsistency();
			sparqlInConsistentInference.runSPARQLInferenceMode();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * Runs sparql query via exampleBooks ontology to query data stored in 'books' Postgresql database.
	 * 
	 * @throws Exception
	 */
	public void runSPARQLInferenceMode() throws Exception {

		OWLOntology ontology = OWLManager.createOWLOntologyManager()
				.loadOntologyFromOntologyDocument(new File(owlFile));

		OBDAModel obdaModel = new MappingLoader().loadFromOBDAFile(obdaFile);

		QuestOWLFactory factory = new QuestOWLFactory();

		QuestOWLConfiguration config = QuestOWLConfiguration.builder().obdaModel(obdaModel).build();

		String sparqlQuery = Files.lines(Paths.get(sparqlFile)).collect(joining("\n"));

		try(QuestOWL reasoner = factory.createReasoner(ontology, config);
			QuestOWLConnection conn = reasoner.getConnection();
			QuestOWLStatement st = conn.createStatement();
			QuestOWLResultSet rs = st.executeTuple(sparqlQuery)) {
			
		int columnSize = rs.getColumnCount();
		
		BufferedWriter bufferOutput = new BufferedWriter(new FileWriter("./resources/sparql_inconsistent_postgresql.txt"));
			
		bufferOutput.write("reasoner.isQuestConsistent() :" + reasoner.isQuestConsistent());
			
		bufferOutput.write("\n");
			
		while (rs.nextRow()) {
			
		for (int i = 1; i <= columnSize; i++){
			
		OWLObject result = rs.getOWLObject(i);			
			
		bufferOutput.write(result.toString());
			
		bufferOutput.write("\n");
			
		}
				
		System.out.print("\n");
				
		}

		bufferOutput.close();			
		rs.close();
		
		}
	}
}