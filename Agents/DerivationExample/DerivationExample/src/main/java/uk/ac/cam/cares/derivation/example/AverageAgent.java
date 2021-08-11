package uk.ac.cam.cares.derivation.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This is an example of an agent updating a derivation that has a time series data
 * The derivation needs to be initialised with createDerivationWithTimeSeries
 * It is not required to give a proper HTTP response to the DerivationClient as this agent did not write any new instances
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {AverageAgent.URL_AVERAGE})
public class AverageAgent extends JPSAgent{
	private static final long serialVersionUID = 1L;
	public static final String URL_AVERAGE = "/AverageAgent";

	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		
		// set up remote store client to point to triple store
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    	SparqlClient sparqlClient = new SparqlClient(storeClient);
    	
		if (validateInput(requestParams, sparqlClient)) {
			JSONArray inputs = requestParams.getJSONArray(DerivationClient.AGENT_INPUT_KEY);
			String inputdata_iri = inputs.getString(0);
			
			if (InstancesDatabase.Average == null) {
				InstancesDatabase.Average = sparqlClient.getAverageIRI();
			}
			
			TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
			
			// get average from input
			double average = tsClient.getAverage(inputdata_iri);
			
			// update average table
			List<Instant> time_column = Arrays.asList(Instant.now());
	    	List<List<?>> values = new ArrayList<>();
	    	List<Double> value_column = Arrays.asList(average);
	    	values.add(value_column);
			TimeSeries<Instant> ts = new TimeSeries<Instant>(time_column, Arrays.asList(InstancesDatabase.Average), values);
			
			tsClient.addTimeSeriesData(ts);
		}
		return requestParams;
	}
	
	private boolean validateInput(JSONObject requestParams, SparqlClient sparqlClient) {
		boolean valid = false;
		try {
			JSONArray inputs = requestParams.getJSONArray(DerivationClient.AGENT_INPUT_KEY);
			
			if (inputs.length() == 1) {
    			if (sparqlClient.isInputData(inputs.getString(0))) {
    				valid = true;
    			} else {
    				throw new BadRequestException("Incorrect input rdf:type");
    			}
    		} else {
    			throw new BadRequestException("Incorrect number of inputs, consider resetting instances");
    		}
			return valid;
		} catch (Exception e) {
			throw new BadRequestException(e);
		}
	}
}
