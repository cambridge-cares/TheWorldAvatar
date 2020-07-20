package uk.ac.cam.cares.jps.base.query.test;

import static org.junit.Assert.*;

import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService.RDFStoreType;

public class KnowledgeBaseClientTest {
	@Test
	public void uploadSingleOntologyTest() throws Exception {
//		 new KnowledgeBaseClient().uploadOntology("http://localhost:8080/blazegraph", "ontokin", "C:/Users/msff2/Documents/c4e-ProciPaper/ACSOmega/OpenData/ABF.owl");
	}
	
	@Test
	public void uploadOntologiesTest() throws Exception {
//		new KnowledgeBaseClient().uploadOntologies("http://localhost:8080/blazegraph", "ontokin", "C:/Users/msff2/Documents/c4e-ProciPaper/ACSOmega/OpenDataTest");
	}
	
	@Test
	public void queryNumberOfMechanisms() throws Exception{
//		String result = new KnowledgeBaseClient().query("http://localhost:8080/blazegraph", "ontokin", RDFStoreType.BLAZEGRAPH, formMechanismCountCountQuery());
//		System.out.println(result);
	}
	
	@Test
	public void queryAllMechanismIRIs() throws Exception{
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient();
		String result = kbClient.query("http://localhost:8080/blazegraph", "ontokin", RDFStoreType.BLAZEGRAPH, formMechanismIRIsQuery());
		System.out.println(result);
	}
	
	/**
	 * A SPARQL query to count the total number of mechanisms in a repository.
	 * 
	 * @return
	 */
	private static String formMechanismCountCountQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT (COUNT(?x) AS ?count)\n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("?x rdf:type ontokin:ReactionMechanism .\n");
			query = query.concat("}\n");
			return query;
	}
	
	/**
	 * A SPARQL query to retrieve the IRIs of all mechanisms in a repository.
	 * 
	 * @return
	 */
	private static String formMechanismIRIsQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT ?x ?y ?z \n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("?x ?y ?z .\n");
			query = query.concat("} LIMIT 10\n");
			return query;
	}
}
