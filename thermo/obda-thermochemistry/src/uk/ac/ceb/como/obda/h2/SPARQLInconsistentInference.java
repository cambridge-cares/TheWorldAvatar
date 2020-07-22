package uk.ac.ceb.como.obda.h2;
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
 */
public class SPARQLInconsistentInference {

	final String owlFile = "./resources/books/exampleBooks.owl";
	final String obdaFile = "./resources/books/bk_code_audio_book_inference_inconsistency.obda";
	final String sparqlFile = "./resources/books/book_id_audio_and_ebook.rq";

	public static void main(String[] args) {
		try {
			SPARQLInconsistentInference sparqlInConsistentInference = new SPARQLInconsistentInference();
			sparqlInConsistentInference.runSPARQLInferenceMode();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void runSPARQLInferenceMode() throws Exception {

		OWLOntology ontology = OWLManager.createOWLOntologyManager()
				.loadOntologyFromOntologyDocument(new File(owlFile));

		OBDAModel obdaModel = new MappingLoader().loadFromOBDAFile(obdaFile);

		QuestOWLFactory factory = new QuestOWLFactory();

		QuestOWLConfiguration config = QuestOWLConfiguration.builder().obdaModel(obdaModel).build();

		String sparqlQuery = Files.lines(Paths.get(sparqlFile)).collect(joining("\n"));

		try (QuestOWL reasoner = factory.createReasoner(ontology, config);
				QuestOWLConnection conn = reasoner.getConnection();
				QuestOWLStatement st = conn.createStatement();
				QuestOWLResultSet rs = st.executeTuple(sparqlQuery)) {
			int columnSize = rs.getColumnCount();
			BufferedWriter bufferOutput = new BufferedWriter(
					new FileWriter("./resources/example/sparql_inconsistent_inference_result_1.txt"));
			bufferOutput.write("reasoner.isQuestConsistent() :" + reasoner.isQuestConsistent());
			bufferOutput.write("\n");
			while (rs.nextRow()) {

				for (int i = 1; i <= columnSize; i++) {
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
