package uk.ac.cam.cares.derivation.example;

import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.annotation.WebServlet;

/**
 * This agent takes two inputs (MinValue and MaxValue), calculate the difference between them, and write the value in the KG
 * @author Kok Foong Lee
 */
@WebServlet(urlPatterns = {DifferenceAgent.URL_Difference})
public class DifferenceAgent extends JPSAgent {
	private static final long serialVersionUID = 1L;

	// ============================ Static variables ===========================
    private static final Logger LOGGER = LogManager.getLogger(DifferenceAgent.class);
    public static final String URL_Difference = "/DifferenceAgent";

    // ================================ Methods ================================
    /**
     * Processes HTTP requests.
     *
     * @param requestParams Request parameters in a JSONObject
     * @param request HTTP Servlet Request
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject response = new JSONObject();
        Config.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    	SparqlClient sparqlClient = new SparqlClient(storeClient);

        if (validateInput(requestParams,sparqlClient)) {
        	JSONArray inputs = requestParams.getJSONArray(DerivationClient.AGENT_INPUT_KEY);
    		LOGGER.info("Calculating difference");
    		Integer minvalue_input = null; Integer maxvalue_input = null;

    		// validate input should already ensure that one of them is a max value and the other is a min value
    		if (sparqlClient.isMaxValue(inputs.getString(0))) {
    			maxvalue_input = sparqlClient.getValue(inputs.getString(0));
    			minvalue_input = sparqlClient.getValue(inputs.getString(1));
    		} else if (sparqlClient.isMinValue(inputs.getString(0))) {
    			minvalue_input = sparqlClient.getValue(inputs.getString(0));
    			maxvalue_input = sparqlClient.getValue(inputs.getString(1));
    		}
    		
    		// calculate a new value and create a new instance
    		int difference = maxvalue_input - minvalue_input;
    		List<String> createdInstances = new ArrayList<>();
    		createdInstances.add(sparqlClient.createDifference());
    		createdInstances.add(sparqlClient.addValueInstance(createdInstances.get(0), difference));
    		LOGGER.info("Created a new calculated difference instance <" + createdInstances.get(0) + ">");
    		response.put(DerivationClient.AGENT_OUTPUT_KEY, new JSONArray(createdInstances));
	       }
        
        return response;
    }

    private boolean validateInput(JSONObject requestParams, SparqlClient sparqlClient) throws BadRequestException {
        boolean valid = false;
        JSONArray inputs = requestParams.getJSONArray(DerivationClient.AGENT_INPUT_KEY);
		LOGGER.info("Checking inputs for DifferenceAgent");
		
		// if the first input is max value, the second one must be min value, and vice versa
		if (inputs.length() == 2) {
			if (sparqlClient.isMaxValue(inputs.getString(0))) {
				if (sparqlClient.isMinValue(inputs.getString(1))) {
					valid = true;
				}
			} else if (sparqlClient.isMinValue(inputs.getString(0))) {
				if (sparqlClient.isMaxValue(inputs.getString(1))) {
					valid = true;
				}
			}
		} else {
			LOGGER.error("Incorrect number of inputs");
			throw new BadRequestException("Incorrect number of inputs");
		}
        
        return valid;
    }

}
