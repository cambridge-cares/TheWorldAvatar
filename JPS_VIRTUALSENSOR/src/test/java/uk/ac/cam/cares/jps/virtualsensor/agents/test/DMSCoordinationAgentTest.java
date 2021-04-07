package uk.ac.cam.cares.jps.virtualsensor.agents.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.virtualsensor.coordination.DMSCoordinationAgent;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;

public class DMSCoordinationAgentTest extends TestCase {
	// local parameters
    String episodeIRI = "http://www.theworldavatar.com/kb/agents/Service__Episode.owl#Service";
    String admsIRI = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
    String keyAgent = "agent";
    String keyReactionmechanism = "reactionmechanism";
    String mech_none = "none";
    String mech_PRF = "http://www.theworldavatar.com/kb/ontokin/Reduced_PRF_ERC_particle.owl#ReactionMechanism_184144363244001";

    /**
     * There are four main tests that will run the full ADMS/Episode simulations:
     * 1) testSingaporeADMS, 2) testSingpaporeEpisode, 3) testHongKongADMS, 4) testHongKongEpisode
     */
	public void testSingaporeADMS() {
		// Runs the full ADMS simulation for Singapore
		JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,1);
		jo.put(keyAgent,admsIRI);
		jo.put(keyReactionmechanism, mech_none);
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/DMSCoordinationAgent",jo.toString());
	}

	public void testSingaporeEpisode() {
		// Runs the full Episode simulation for Singapore
		JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,2);
		jo.put(keyAgent,episodeIRI);
		jo.put(keyReactionmechanism, mech_none);
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/DMSCoordinationAgent",jo.toString());
	}

	public void testHongKongADMS() {
		// Runs the full ADMS simulation for Hong Kong
		JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,3);
		jo.put(keyAgent,admsIRI);
		jo.put(keyReactionmechanism, mech_none);
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/DMSCoordinationAgent",jo.toString());
	}

	public void testHongKongEpisode() {
		// Runs the full Episode simulation for Hong Kong
		JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,4);
		jo.put(keyAgent,episodeIRI);
		jo.put(keyReactionmechanism, mech_none);
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/DMSCoordinationAgent",jo.toString());
	}
	
	public void testPlymouthEpisode() {
		JSONObject jo = new JSONObject();
		Region.putRegionAndStation(jo,5);
		jo.put(keyAgent,episodeIRI);
		jo.put(DispSimSparql.SimKey, "http://www.theworldavatar.com/kb/ontodispersionsim/OntoDispersionSim.owl#sim5");
		AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/DMSCoordinationAgent",jo.toString());
	}
}
