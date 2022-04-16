package uk.ac.cam.cares.derivation.example;

import java.time.Instant;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * this agent queries the minimum value from an input time series table using TimeSeriesClient and writes a new MinValue instance in the KG
 * In the HTTP response, it writes the newly created instances so that the DerivationClient knows what instances to link 
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {MinValueAgent.URL_MINVALUE})
public class MinValueAgent extends DerivationAgent {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final String URL_MINVALUE = "/MinValueAgent";
	private static final Logger LOGGER = LogManager.getLogger(MinValueAgent.class);
	
	@Override
	public DerivationOutputs processRequestParameters(DerivationInputs derivationInputs) {
		LOGGER.info("Received request: " + derivationInputs.toString());
        Config.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    	SparqlClient sparqlClient = new SparqlClient(storeClient);
    	
    	if (validateInput(derivationInputs, sparqlClient)) {
    		String inputdata_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.InputData)).get(0);
    		
    		// query from RDB using TimeSeries Client
    		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
    		Integer minvalue = (int) tsClient.getMinValue(inputdata_iri);
			
    		// create new instances in KG
			String createdMin = sparqlClient.createMinValue();
			sparqlClient.addValueInstance(createdMin, minvalue);
			
			// inform new instances created to the DerivationClient
			DerivationOutputs derivationOutputs = new DerivationOutputs(
				SparqlClient.getRdfTypeString(SparqlClient.MinValue), createdMin);
			LOGGER.info("Created a new min value instance <" + createdMin + ">");
			return derivationOutputs;
    	} else {
			throw new BadRequestException("Input validation failed.");
		}
	}
	
	private boolean validateInput(DerivationInputs derivationInputs, SparqlClient sparqlClient) {
		boolean valid = false;
        LOGGER.debug("Checking input for MaxValue agent");
		
		List<String> inputData = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.InputData));
		if (inputData.size() == 1) {
			String inputDataIri = inputData.get(0);
			if (sparqlClient.isInputData(inputDataIri)) {
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
