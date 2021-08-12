package uk.ac.cam.cares.derivation.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
	
	private static final Logger LOGGER = LoggerFactory.getLogger(InitialiseInstances.class);
    
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
    	createInputTimeSeries(input, tsClient);
    	LOGGER.info("created input " + input);
    	InstancesDatabase.Input = input;
    	
    	String min_property = sparqlClient.createMinValue();
    	String min_value = sparqlClient.addValueInstance(min_property, 0);
    	LOGGER.info("created min value " + min_property);
    	
    	String max_property = sparqlClient.createMaxValue();
    	String max_value = sparqlClient.addValueInstance(max_property, 0);
    	LOGGER.info("created max value " + max_property);
    	
    	String diff_property = sparqlClient.createDifference();
    	String diff_value = sparqlClient.addValueInstance(diff_property, 0);
    	LOGGER.info("created calculated difference " + diff_property);
    	
    	String average = sparqlClient.createAverage();
    	LOGGER.info("created average " + average);
    	tsClient.initTimeSeries(Arrays.asList(average), Arrays.asList(Double.class), null);
    	LOGGER.info("initialise a table for average values");
    	InstancesDatabase.Average = average;
    	
    	// create 3 standard derived quantities
    	String derived_minvalue = devClient.createDerivation(Arrays.asList(min_property,min_value), minvalue_agent_iri, minvalue_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for min value " + derived_minvalue);
    	
    	String derived_maxvalue = devClient.createDerivation(Arrays.asList(max_property,max_value), maxvalue_agent_iri, maxvalue_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for max value " + derived_maxvalue);
    	
    	String derived_difference = devClient.createDerivation(Arrays.asList(diff_property,diff_value), difference_agent_iri, difference_agent_url, Arrays.asList(min_property,max_property));
    	LOGGER.info("created derived quantity for calculated difference " + derived_difference);
    	InstancesDatabase.DerivedDifference = derived_difference;
    	
    	// average is a derivation with a time series
    	String derived_average = devClient.createDerivationWithTimeSeries(average, average_agent_iri, average_agent_url, Arrays.asList(input));
    	LOGGER.info("created derivation for average " + derived_average);
    	InstancesDatabase.DerivedAverage = derived_average;
    	
    	// check all connections between the derived quantities
    	// as calculated difference is derived from min value and max value, they get checked too
    	// the validate method only traverse down, not up
    	LOGGER.info("Validating " + derived_difference);
    	try {
    		devClient.validateDerivation(derived_difference);
    		LOGGER.info("Validated derived difference successfully");
    	} catch (Exception e) {
    		LOGGER.error("Validation failure for derived difference" + e.getMessage());
    		throw new JPSRuntimeException(e);
    	}
    	
    	LOGGER.info("Validating " + derived_average);
    	try {
    		devClient.validateDerivation(derived_average);
    		LOGGER.info("Validated derived average successfully");
    	} catch (Exception e) {
    		LOGGER.error("Validation failure for derived average: " + e.getMessage());
    		throw new JPSRuntimeException(e);
    	}

    	return requestParams;
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
