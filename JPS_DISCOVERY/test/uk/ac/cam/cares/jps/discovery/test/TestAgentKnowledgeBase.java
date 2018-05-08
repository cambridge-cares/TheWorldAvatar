package uk.ac.cam.cares.jps.discovery.test;

import java.io.UnsupportedEncodingException;

import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.config.AgentLocator;
import uk.ac.cam.cares.jps.discovery.knowledgebase.AgentKnowledgeBase;

public class TestAgentKnowledgeBase extends TestCase {

	public void xxxtestAgentImportToKnowledgeBase() {
		
		String sparql = "PREFIX jpsago:<http://www.jparksimulator.com/OntoAgent.owl#> "
				+ "SELECT ?id "
				+ "WHERE {  a jpsago:Agent ."
				+ "?agent jpsago:hasId ?id"
				+ "}";
		
		sparql = "PREFIX jpsago:<http://www.jparksimulator.com/OntoAgent.owl#> "
				+ "SELECT ?s ?p ?o "
				+ "WHERE {  ?s ?p ?o ."
				+ "}";
		ResultSet rs = AgentKnowledgeBase.query(sparql);
		while(rs.hasNext()) {
			QuerySolution sol = rs.nextSolution();
			String id = sol.getLiteral("s").getString();
			System.out.println("MY ID =" + id);
		}
		
		
//		String IdSource = "PREFIX rns:<http://www.jparksimulator.com/ResourceNetworkSimplified.owl#> "
//				+ "SELECT ?source ?IRI "
//				+ "WHERE { ?entity a rns:ResourceNetwork ."
//				+ "?entity rns:hasSources ?s_i ."
//				+ "?s_i rns:hasName ?source ."
//				+ "?s_i rns:hasIRI ?IRI ."
//				+ "FILTER regex (str(?entity), '"+ nameOfResourceNetwork +"') ."
//				+ "}"
//				+ "ORDER BY ?sources DESC(?added)"
//				;
	}
	

}
