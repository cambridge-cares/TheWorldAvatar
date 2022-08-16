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

	public static void main(String[] args) {
		// String kgurl = "http://localhost:8889/blazegraph/namespace/kb/sparql";
		// Config.initProperties();
		// RemoteStoreClient storeClient = new RemoteStoreClient(kgurl, kgurl,
		// Config.kguser,
		// Config.kgpassword);
		// DerivationClient devClient = new DerivationClient(storeClient,
		// InitialiseInstances.derivationInstanceBaseURL);
		// JSONObject initResponse = new JSONObject(
		// AgentCaller.executeGet("http://localhost:8081/DerivationExample/InitialiseInstances"));
		// devClient.updatePureSyncDerivation(initResponse.getString(InitialiseInstances.min_dev_key));
		JSONObject initResponse = new JSONObject(
				AgentCaller.executeGet("http://localhost:8081/DerivationExample/InitialiseInstances"));
		String input = initResponse.getString(InitialiseInstances.input_key);
		JSONObject request = new JSONObject()
				.put(DerivationClient.AGENT_INPUT_KEY,
						new JSONObject().put(SparqlClient.getRdfTypeString(SparqlClient.InputData), input))
				.put(DerivationClient.BELONGSTO_KEY,
						new JSONObject().put(initResponse.getString(InitialiseInstances.min_key),
								SparqlClient.getRdfTypeString(SparqlClient.MinValue)))
				.put(DerivationClient.DERIVATION_KEY, initResponse.getString(InitialiseInstances.min_dev_key))
				.put(DerivationClient.DERIVATION_TYPE_KEY, DerivationSparql.ONTODERIVATION_DERIVATION)
				.put(DerivationClient.DOWNSTREAMDERIVATION_KEY, new JSONObject().put(
						initResponse.getString(InitialiseInstances.min_key),
						new JSONArray(Arrays.asList(initResponse.getString(InitialiseInstances.diff_dev_key)))));

		String response = AgentCaller.executeGetWithURLAndJSON("http://localhost:8081/DerivationExample/MinValueAgent",
				request.toString());
		JSONObject responseJson = new JSONObject(response);
		System.out.println(responseJson);
	}
}
