package uk.ac.cam.cares.derivation.asynexample;

import java.util.Arrays;

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
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.sparqlEndpoint, Config.sparqlEndpoint, Config.kgUser, Config.kgPassword);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		DerivationClient devClient = new DerivationClient(storeClient, Config.derivationInstanceBaseURL);
		
		JSONObject response = updateDerivations(sparqlClient, devClient);

		return response;
	}

	JSONObject updateDerivations(SparqlClient sparqlClient, DerivationClient devClient) {
		String difference = sparqlClient.getDifferenceIRI();
		String difference_derivation = devClient.getDerivationsOf(Arrays.asList(difference)).get(difference);
		
		devClient.updateDerivationAsyn(difference_derivation);
		
		String res_msg = "Checked derivation of difference <" + difference_derivation + ">, the update should be done in a few minutes";
		LOGGER.info(res_msg);
		
		JSONObject response = new JSONObject();
		response.put("status", res_msg);
		
		return response;
	}
}
