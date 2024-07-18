package uk.ac.cam.cares.derivation.asynexample;

import java.util.Arrays;
import java.util.List;

import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This is the entry point of the asynchronous derivation example.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = {UpdateDerivations.API_PATTERN})
public class UpdateDerivations extends JPSAgent {
	private static final long serialVersionUID = 1L;
	
	private static final Logger LOGGER = LogManager.getLogger(UpdateDerivations.class);
	
	static final String API_PATTERN = "/UpdateDerivations";
	static final String DIFFERENCE_DERIVATION_KEY = "DifferenceDerivation";
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
		DerivationClient devClient = new DerivationClient(storeClient, Config.derivationInstanceBaseURL);

		if (!requestParams.has(DIFFERENCE_DERIVATION_KEY)) {
			return updateDerivations(devClient.getDerivations(Config.agentIriDifference), devClient);
		} else {
			return updateDerivations(requestParams.getString(DIFFERENCE_DERIVATION_KEY), devClient);
		}
	}

	JSONObject updateDerivations(String differenceDerivation, DerivationClient devClient) {
		return updateDerivations(Arrays.asList(differenceDerivation), devClient);
	}

	JSONObject updateDerivations(List<String> differenceDerivations, DerivationClient devClient) {
		differenceDerivations.stream().forEach(devClient::unifiedUpdateDerivation);

		String res_msg = "Checked derivation of difference <" + differenceDerivations + ">, the update should be done in a few minutes";
		LOGGER.info(res_msg);

		JSONObject response = new JSONObject();
		response.put("status", res_msg);

		return response;
	}
}
