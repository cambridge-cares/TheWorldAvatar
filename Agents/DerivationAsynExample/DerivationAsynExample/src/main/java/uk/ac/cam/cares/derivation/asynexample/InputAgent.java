package uk.ac.cam.cares.derivation.asynexample;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This input agent adds value to the UpperLimit, LowerLimit, and NumberOfPoints. Then updates the timestamp.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = {InputAgent.API_PATTERN})
public class InputAgent extends JPSAgent {
	// this agent should be a servlet that developer can send request to give the inputs
	
	private static final long serialVersionUID = 1L;
	
	private static final Logger LOGGER = LogManager.getLogger(InputAgent.class);
	
	static final String API_PATTERN = "/InputAgent";
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.sparqlEndpointQuery, Config.sparqlEndpointUpdate);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		DerivationClient devClient = new DerivationClient(storeClient);
		
		// get the input from the request
		Integer upperLimit = Integer.parseInt(requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.UpperLimit.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace)));
		Integer lowerLimit = Integer.parseInt(requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.LowerLimit.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace)));
		Integer numberOfPoints = Integer.parseInt(requestParams.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getString(SparqlClient.NumberOfPoints.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace)));
		
		// update the instances with the new input values, also update the timestamp to make it current
		String ul_iri = sparqlClient.getUpperLimitIRI();
		sparqlClient.updateValue(ul_iri, upperLimit);
		devClient.updateTimestamp(ul_iri);
		
		String ll_iri = sparqlClient.getLowerLimitIRI();
		sparqlClient.updateValue(ll_iri, lowerLimit);
		devClient.updateTimestamp(ll_iri);
		
		String np_iri = sparqlClient.getNumberOfPointsIRI();
		sparqlClient.updateValue(np_iri, numberOfPoints);
		devClient.updateTimestamp(np_iri);
		
		JSONObject response = new JSONObject();
		JSONObject iris = new JSONObject();
		iris.put("UpperLimit instance", ul_iri);
		iris.put("LowerLimit instance", ll_iri);
		iris.put("NumberOfPoints instance", np_iri);
		response.put("Updated successfully", iris);
		
		return response;
	}
}
