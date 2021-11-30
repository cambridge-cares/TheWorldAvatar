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
		
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.sparqlEndpointQuery, Config.sparqlEndpointUpdate);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		DerivationClient devClient = new DerivationClient(storeClient);
		
		String numberOfPoints_iri = sparqlClient.getNumberOfPointsIRI();
		
		// update the NumberOfPoints by adding 1 to its current value, also update the timestamp to make it current
		sparqlClient.updateValue(numberOfPoints_iri, sparqlClient.getValue(numberOfPoints_iri) + 1);
		devClient.updateTimestamp(numberOfPoints_iri);
		
		JSONObject response = new JSONObject();
		JSONObject iris = new JSONObject();
		iris.put("NumberOfPoints instance", numberOfPoints_iri);
		response.put("Updated successfully", iris);
		
		return response;
	}
}
