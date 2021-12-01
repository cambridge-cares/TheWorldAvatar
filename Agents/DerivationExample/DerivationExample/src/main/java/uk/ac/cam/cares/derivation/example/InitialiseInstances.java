package uk.ac.cam.cares.derivation.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


@WebServlet(urlPatterns = {"/InitialiseInstances"})
public class InitialiseInstances extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
    // this URL is based on the docker image, make sure the URL is accessible from the derived quantity's calling entity
	private static String baseURL = "http://derivationexample:8080/DerivationExample";
	
	private static String minvalue_agent_iri = SparqlClient.namespace + "minvalue_agent";
	private static String minvalue_agent_url = baseURL + MinValueAgent.URL_MINVALUE;
	
	private static String maxvalue_agent_iri = SparqlClient.namespace + "maxvalue_agent";
	private static String maxvalue_agent_url = baseURL + MaxValueAgent.URL_MAXVALUE;
	
	private static String difference_agent_iri = SparqlClient.namespace + "difference_agent";
	private static String difference_agent_url = baseURL + DifferenceAgent.URL_Difference;
	
	private static String average_agent_iri = SparqlClient.namespace + "average_agent";
 	private static String average_agent_url = baseURL + AverageAgent.URL_AVERAGE;
	
	private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);
    
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
    	RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    	SparqlClient sparqlClient = new SparqlClient(storeClient);
    	DerivationClient devClient = new DerivationClient(storeClient);
    	
    	LOGGER.info("Initialising new instances, all existing instances will get deleted");
    	sparqlClient.clearKG();
    	TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
    	tsClient.deleteAll();
    	
    	// record the IRIs of the created instances to link them later
    	String input = sparqlClient.createInputData();
    	// attach timestamp to input
    	devClient.addTimeInstance(input);
    	// the timestamp added using addTimeInstance is 0, this will ensure that the input is current
    	devClient.updateTimestamp(input);
    	createInputTimeSeries(input, tsClient);
    	LOGGER.info("Created input <" + input + ">");
    	InstancesDatabase.Input = input;
    	
    	String min_property = sparqlClient.createMinValue();
    	String min_value = sparqlClient.addValueInstance(min_property, 0);
    	LOGGER.info("Created min value <" + min_property + ">");
    	
    	String max_property = sparqlClient.createMaxValue();
    	String max_value = sparqlClient.addValueInstance(max_property, 0);
    	LOGGER.info("Created max value <" + max_property + ">");
    	
    	String diff_property = sparqlClient.createDifference();
    	String diff_value = sparqlClient.addValueInstance(diff_property, 0);
    	LOGGER.info("Created calculated difference <" + diff_property + ">");
    	
    	String average = sparqlClient.createAverage();
    	LOGGER.info("Created average <" + average + ">");
    	tsClient.initTimeSeries(Arrays.asList(average), Arrays.asList(Double.class), null);
    	LOGGER.info("Initialised a table for average values");
    	InstancesDatabase.Average = average;
    	
    	// create 3 standard derived quantities
    	String derived_minvalue = devClient.createDerivation(Arrays.asList(min_property,min_value), minvalue_agent_iri, minvalue_agent_url, Arrays.asList(input));
    	LOGGER.info("Created derived quantity for min value <" + derived_minvalue + ">");
    	
    	String derived_maxvalue = devClient.createDerivation(Arrays.asList(max_property,max_value), maxvalue_agent_iri, maxvalue_agent_url, Arrays.asList(input));
    	LOGGER.info("Created derived quantity for max value <" + derived_maxvalue + ">");
    	
    	String derived_difference = devClient.createDerivation(Arrays.asList(diff_property,diff_value), difference_agent_iri, difference_agent_url, Arrays.asList(min_property,max_property));
    	LOGGER.info("Created derived quantity for calculated difference <" + derived_difference + ">");
    	
    	// average is a derivation with a time series
    	String derived_average = devClient.createDerivationWithTimeSeries(Arrays.asList(average), average_agent_iri, average_agent_url, Arrays.asList(input));
    	LOGGER.info("Created derivation for average <" + derived_average + ">");
    	
    	// check all connections between the derived quantities
    	// as calculated difference is derived from min value and max value, they get checked too
    	// the validate method only traverse down, not up
    	LOGGER.info("Validating " + derived_difference);
    	try {
    		if (devClient.validateDerivations()) {
    			LOGGER.info("Validated derived difference successfully");
    		}
    	} catch (Exception e) {
    		LOGGER.error("Validation failure for derived difference" + e.getMessage());
    		throw new JPSRuntimeException(e);
    	}
    	
    	LOGGER.info("Validating " + derived_average);
    	try {
    		if (devClient.validateDerivations()) {
    			LOGGER.info("Validated derivation of average successfully");
    		}
    	} catch (Exception e) {
    		LOGGER.error("Validation failure for derived average: " + e.getMessage());
    		throw new JPSRuntimeException(e);
    	}

    	JSONObject response = new JSONObject();
    	response.put("input", input);
    	response.put("min value", min_property);
    	response.put("derivation of min value", derived_minvalue);
    	response.put("max value", max_property);
    	response.put("derivation of max value", derived_maxvalue);
    	response.put("difference", diff_property);
    	response.put("derivation of difference", derived_difference);
    	response.put("average", average);
    	response.put("derivation of average", derived_average);
    	
    	return response;
    }
	
    private static void createInputTimeSeries(String input_iri, TimeSeriesClient<Instant> tsClient) {
    	tsClient.initTimeSeries(Arrays.asList(input_iri), Arrays.asList(Integer.class), null);
    	
    	// create a new time series object with random numbers
    	Random rand = new Random();
    	List<Instant> time_column = Arrays.asList(Instant.now());
    	List<List<?>> values = new ArrayList<>();
    	List<Integer> value_column = Arrays.asList(rand.nextInt());
    	values.add(value_column);
    	TimeSeries<Instant> ts = new TimeSeries<Instant>(time_column, Arrays.asList(input_iri), values);
    	
    	tsClient.addTimeSeriesData(ts);
    }
}
