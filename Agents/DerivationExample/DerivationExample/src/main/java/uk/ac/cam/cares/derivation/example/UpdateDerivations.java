package uk.ac.cam.cares.derivation.example;

import java.util.Arrays;

import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@WebServlet(urlPatterns = {"/UpdateDerivations"})
public class UpdateDerivations extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Logger LOGGER = LogManager.getLogger(UpdateDerivations.class);

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		DerivationClient devClient = new DerivationClient(storeClient, InitialiseInstances.derivationInstanceBaseURL);

		// method updateAllSyncDerivations makes use of DerivationInputs/DerivationOutputs/DerivationAgent
		devClient.updateAllSyncDerivations();

		String res_msg = "Updated derivations";
		LOGGER.info(res_msg);

		JSONObject response = new JSONObject();
		response.put("status", res_msg);
		return response;
	}
}
