package uk.ac.cam.cares.derivation.asynexample;

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
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.sparqlEndpointQuery, Config.sparqlEndpointUpdate);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		DerivationClient devClient = new DerivationClient(storeClient);
		
		if (InstanceDatabase.DerivedDifference == null) {
			String difference = sparqlClient.getDifferenceIRI();
			InstanceDatabase.DerivedDifference = devClient.getDerivationOf(difference);
		}
		
		devClient.updateDerivationAsyn(InstanceDatabase.DerivedDifference);
		
		String res_msg = "Checked derivation of difference <" + InstanceDatabase.DerivedDifference + ">, the update should be done in a few minutes";
		LOGGER.info(res_msg);
		
		JSONObject response = new JSONObject();
		response.put("status", res_msg);
		return response;
	}
}
