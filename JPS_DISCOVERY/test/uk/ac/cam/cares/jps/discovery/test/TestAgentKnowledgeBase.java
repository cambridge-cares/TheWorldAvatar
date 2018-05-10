package uk.ac.cam.cares.jps.discovery.test;

import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.config.AgentLocator;
import uk.ac.cam.cares.jps.discovery.knowledgebase.AgentKnowledgeBase;

public class TestAgentKnowledgeBase extends TestCase {

	public void testAgentImportToKnowledgeBase() {
		//TODO-AE getNewRooo.... refactor
		String dirForAgentKnowledgesBase = AgentLocator.getNewRootDirectory(this) + "/testres/fiveagentsowlfiles";
		AgentKnowledgeBase.getInstance(dirForAgentKnowledgesBase);
		
		String sparql = "PREFIX jpsago:<http://www.theworldavatar.com/OntoAgent/OntoAgent.owl#> "
				+ "SELECT * "
				+ "WHERE { "
				+ "?agent a jpsago:Agent ."
				+ "?agent jpsago:hasId ?id ."
				+ "}";

		ResultSet rs = AgentKnowledgeBase.query(sparql);
		
		while(rs.hasNext()) {
			QuerySolution sol = rs.nextSolution();
			Resource agent = sol.getResource("agent");
			Literal id = sol.getLiteral("id");
			System.out.println("MY agent = " + agent + ", id = " + id);
		}
	}
}
