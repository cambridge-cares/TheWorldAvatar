package uk.ac.cam.cares.jps.scenario.kg.test;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.scenario.kg.KnowledgeBaseAgentNew;

public class TestKnowledgeBA  extends TestCase {
	 public void testNewKBAgent() {
	        KnowledgeBaseAgentNew jpsa = null;
	        try {
	            jpsa = new KnowledgeBaseAgentNew();
	        } finally {
	            assertNotNull(jpsa);
	        }
	    }
	public void testAgentRouter() {
		String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
            + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
            + "SELECT ?entity "
            + "WHERE {?entity  a  j1:PowerGenerator  ."
            + "FILTER EXISTS {?entity j2:isSubsystemOf ?plant } " //filtering gen 001 as it is slackbus
            + "}";

		JSONObject jo = new JSONObject()
				.put(JPSConstants.SCENARIO_RESOURCE, "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork")
				.put(JPSConstants.QUERY_SPARQL_QUERY,gennodeInfo  );
//		AgentCaller.executeGetWithJsonParameter("jps/kb/scenarioFolder", jo.toString());

        KnowledgeBaseAgentNew jpsa = new KnowledgeBaseAgentNew();
        jpsa.main(jo);
	}
	 
}
