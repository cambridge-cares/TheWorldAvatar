package uk.ac.cam.cares.derivation.example;

import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.List;

import javax.servlet.annotation.WebServlet;

/**
 * This agent takes two inputs (MinValue and MaxValue), calculate the difference between them, and write the value in the KG
 * @author Kok Foong Lee
 */
@WebServlet(urlPatterns = {DifferenceAgent.URL_Difference})
public class DifferenceAgent extends DerivationAgent {
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
    public DerivationOutputs processRequestParameters(DerivationInputs derivationInputs) {
        Config.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    	SparqlClient sparqlClient = new SparqlClient(storeClient);

        if (validateInput(derivationInputs,sparqlClient)) {
        	LOGGER.info("Calculating difference");

			// validate input should already ensure that both max and min value exist
			String maxIri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MaxValue)).get(0);
			String minIri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MinValue)).get(0);
    		Integer minvalue_input = null; Integer maxvalue_input = null;
			maxvalue_input = sparqlClient.getValue(maxIri);
			minvalue_input = sparqlClient.getValue(minIri);
    		
    		// calculate a new value and create a new instance
    		int difference = maxvalue_input - minvalue_input;
    		String createdDifference = sparqlClient.createDifference();
			sparqlClient.addValueInstance(createdDifference, difference);
    		LOGGER.info("Created a new calculated difference instance <" + createdDifference + ">");
			// create DerivationOutputs instance
			DerivationOutputs derivationOutputs = new DerivationOutputs(
				SparqlClient.getRdfTypeString(SparqlClient.Difference), createdDifference);
			return derivationOutputs;
	    } else {
			LOGGER.error("Input validation failed.");
			throw new BadRequestException("Input validation failed.");
		}
    }

    private boolean validateInput(DerivationInputs derivationInputs, SparqlClient sparqlClient) throws BadRequestException {
        boolean valid = false;
		LOGGER.info("Checking inputs for DifferenceAgent");
		
		// check if the two inputs are complete
		List<String> max = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MaxValue));
		List<String> min = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MinValue));
		if (max.size() == 1 && min.size() == 1) {
			String maxIri = max.get(0);
			String minIri = min.get(0);
			if (sparqlClient.isMaxValue(maxIri)) {
				if (sparqlClient.isMinValue(minIri)) {
					valid = true;
				} else {
					LOGGER.error("MinValue IRI passed in doesn't match MinValue rdf:type.");
					throw new BadRequestException("MinValue IRI passed in doesn't match MinValue rdf:type.");
				}
			} else {
				LOGGER.error("MaxValue IRI passed in doesn't match MaxValue rdf:type.");
				throw new BadRequestException("MaxValue IRI passed in doesn't match MaxValue rdf:type.");
			}
		} else {
			LOGGER.error("Incorrect number of inputs");
			throw new BadRequestException("Incorrect number of inputs");
		}
        
        return valid;
    }

}
