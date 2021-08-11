package uk.ac.cam.cares.derivation.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * this agent queries the minimum value from an input time series table using TimeSeriesClient and writes a new MinValue instance in the KG
 * In the HTTP response, it writes the newly created instances so that the DerivationClient knows what instances to link 
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {MinValueAgent.URL_MINVALUE})
public class MinValueAgent extends JPSAgent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final String URL_MINVALUE = "/MinValueAgent";
	private static final Logger LOGGER = LoggerFactory.getLogger(MinValueAgent.class);
	
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject response = new JSONObject();
        Config.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    	SparqlClient sparqlClient = new SparqlClient(storeClient);
    	
    	if (validateInput(requestParams, sparqlClient)) {
    		String inputdata_iri = requestParams.getJSONArray(DerivationClient.AGENT_INPUT_KEY).getString(0);
    		
    		// query from RDB using TimeSeries Client
    		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
    		Integer minvalue = (int) tsClient.getMinValue(inputdata_iri);
			
    		// create new instances in KG
    		List<String> createdInstances = new ArrayList<>();
    		createdInstances.add(sparqlClient.createMinValue());
			createdInstances.add(sparqlClient.addValueInstance(createdInstances.get(0), minvalue));
			
			// inform new instances created to the DerivationClient
			response.put(DerivationClient.AGENT_OUTPUT_KEY, new JSONArray(createdInstances));
			LOGGER.info("created a new min value instance " + createdInstances);
    	}
    	
    	return response;
	}
	
	private boolean validateInput(JSONObject requestParams, SparqlClient sparqlClient) {
		boolean valid = false;
        JSONArray inputs = requestParams.getJSONArray(DerivationClient.AGENT_INPUT_KEY);
        LOGGER.info("Checking input for MinValue agent");
		
		if (inputs.length() == 1) {
			if (sparqlClient.isInputData(inputs.getString(0))) {
				valid = true;
			} else {
				throw new BadRequestException("Incorrect rdf:type for input");
			}
		} else {
			throw new BadRequestException("Incorrect number of inputs");
		}
		
		return valid;
	}
}
