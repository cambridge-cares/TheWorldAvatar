package uk.ac.cam.cares.jps.ship.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;

public class TestADMSCoordinationAgent extends TestCase {

	public void testADMSCoordinationAgentForShipWithinScenario() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testADMSCoordinationAgentForShipWithinScenario");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		
		JSONObject upcorn = new JSONObject();
		upcorn.put("upperx", "12708200.45");
		upcorn.put("uppery", "2546850.028");
		JSONObject lowcorn = new JSONObject();
		lowcorn.put("lowerx", "12706630.262");
		lowcorn.put("lowery", "2545539.172");
		JSONObject joregion = new JSONObject();
		joregion.put("srsname","EPSG:3857");
		joregion.put("lowercorner",lowcorn);
		joregion.put("uppercorner",upcorn);
		
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service");
		jo.put("region", joregion);
		jo.put("location", "Hong Kong");
		jo.put("reactionmechanism", "http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001");

		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/ADMSCoordinationAgentForShipWithoutComposition",jo.toString());
		System.out.println(resultStart);
	}
}
