package uk.ac.cam.cares.jps.discovery.test;

import java.util.Collection;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Resource;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentServiceDescription;
import uk.ac.cam.cares.jps.discovery.knowledgebase.AgentKnowledgeBase;
import uk.ac.cam.cares.jps.discovery.knowledgebase.JenaHelper;

public class TestAgentKnowledgeBase extends TestCase {

	public void testImportFiveAgentsToKnowledgeBase() {
		
		AgentKnowledgeBase base = AgentKnowledgeBase.createNewInstanceWithoutReading();
		String path = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres/fiveagentsowlfiles";
		base.read(path);
		
		String sparql = "PREFIX jpsago:<http://www.theworldavatar.com/OntoAgent/OntoAgent.owl#> "
				+ "SELECT * "
				+ "WHERE { "
				+ "?agent a jpsago:Agent ."
				+ "?agent jpsago:hasId ?id ."
				+ "}";

		ResultSet rs = base.query(sparql);
		
		int actualAgentCount = 0;
		while(rs.hasNext()) {
			QuerySolution sol = rs.nextSolution();
			Resource agent = sol.getResource("agent");
			Literal id = sol.getLiteral("id");
			if ((agent != null) & (id != null)) {
				actualAgentCount++;
			}
		}
		
		assertEquals(5, actualAgentCount);
	}
	
	public void testQueryAgentWithGivenName() {
		
		AgentKnowledgeBase base = AgentKnowledgeBase.createNewInstanceWithoutReading();
		String path = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres/fiveagentsowlfiles";
		base.read(path);
		
		Collection<Agent> agents = base.getAllAgents();
		
		Agent found = null;
		for (Agent current : agents) {
			if (current.getName().equals("IRIAgent1")) {
				found = current;
				break;
			}
		}
		
		AgentServiceDescription actual = found.getDescriptions().get(0);
		assertEquals("domain", actual.getProperties().get(0).getKey());
		assertEquals("building", actual.getProperties().get(0).getValue());
		assertEquals("street", actual.getInputParameters().get(0).getKey());
		assertEquals(null, actual.getInputParameters().get(0).getValue());
		assertEquals("IRIHeight", actual.getOutputParameters().get(0).getKey());
		assertEquals(null, actual.getOutputParameters().get(0).getValue());
	}
	
	public void testImportFileFromURL() {
		OntModel model = JenaHelper.createModel("http://www.theworldavatar.com/BMS/OLA.owl");		
	}
}
