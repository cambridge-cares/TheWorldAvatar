package uk.ac.cam.cares.derivation.example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.derivation.config.Config;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
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
	private static String baseURL = "http://derivationagent:8080/DerivationAgent";
	
	private static String minvalue_agent_iri = SparqlClient.namespace + "minvalue_agent";
	private static String minvalue_agent_url = baseURL + DerivationAgents.URL_MINVALUE;
	
	private static String maxvalue_agent_iri = SparqlClient.namespace + "maxvalue_agent";
	private static String maxvalue_agent_url = baseURL + DerivationAgents.URL_MAXVALUE;
	
	private static String difference_agent_iri = SparqlClient.namespace + "difference_agent";
	private static String difference_agent_url = baseURL + DerivationAgents.URL_CalculatedDifference;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(InitialiseInstances.class);
    
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
    	RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    	SparqlClient sparqlClient = new SparqlClient(storeClient);
    	DerivationClient devClient = new DerivationClient(storeClient);
    	
    	System.out.println("Initialising new instances, all existing instances will get deleted");
    	sparqlClient.clearKG();
    	TimeSeriesClient<Integer> tsClient = new TimeSeriesClient<Integer>(storeClient, Integer.class, Config.dburl, Config.dbuser, Config.dbpassword);
    	tsClient.deleteAll();
    	
    	// record the IRIs of the created instances to link them later
    	String input = sparqlClient.createInputData();
    	// attach timestamp to input
    	devClient.addTimeInstance(input);
    	createTimeSeries(input, tsClient);
    	LOGGER.info("created input " + input);
    	InstancesDatabase.Input = input;
    	
    	String[] minvalue = sparqlClient.createMinValue(0);
    	LOGGER.info("created min value " + minvalue[0]);
    	InstancesDatabase.MinValue = minvalue[0];
    	
    	String[] maxvalue = sparqlClient.createMaxValue(0);
    	LOGGER.info("created max value " + maxvalue[0]);
    	InstancesDatabase.MaxValue = maxvalue[0];
    	
    	String[] difference = sparqlClient.createCalculatedDifference(0);
    	LOGGER.info("created calculated difference " + difference[0]);
    	InstancesDatabase.Difference = difference[0];
    	
    	// create three derived quantities
    	String derived_minvalue = devClient.createDerivation(Arrays.asList(minvalue), minvalue_agent_iri, minvalue_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for min value " + derived_minvalue);
    	InstancesDatabase.DerivedQuantityMinValue = derived_minvalue;
    	
    	String derived_maxvalue = devClient.createDerivation(Arrays.asList(maxvalue), maxvalue_agent_iri, maxvalue_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for max value " + derived_maxvalue);
    	InstancesDatabase.DerivedQuantityMaxValue = derived_maxvalue;
    	
    	String derived_difference = devClient.createDerivation(Arrays.asList(difference), difference_agent_iri, difference_agent_url, Arrays.asList(minvalue[0],maxvalue[0]));
    	LOGGER.info("created derived quantity for calculated difference " + derived_difference);
    	InstancesDatabase.DerivedQuantityDifference = derived_difference;
    	
    	// check all connections between the derived quantities
    	// as calculated difference is derived from min time and max time, they get checked too
    	// the validate method only traverse down, not up
    	LOGGER.info("Validating " + derived_difference);
    	if (devClient.validateDerivation(derived_difference)) {
    		LOGGER.info("validation success");
    	} else {
    		LOGGER.error("ERROR: validation fail");
    	}

    	return requestParams;
    }
	
    private static void createTimeSeries(String input_iri, TimeSeriesClient<Integer> tsClient) {
    	tsClient.initTimeSeries(Arrays.asList(input_iri), Arrays.asList(Integer.class), null);
    	
    	// create a new time series object with random numbers
    	Random rand = new Random();
    	List<Integer> time_column = Arrays.asList(1,2);
    	List<List<?>> values = new ArrayList<>();
    	List<Integer> value_column = Arrays.asList(rand.nextInt(),rand.nextInt());
    	values.add(value_column);
    	TimeSeries<Integer> ts = new TimeSeries<Integer>(time_column, Arrays.asList(input_iri), values);
    	
    	tsClient.addTimeSeriesData(ts);
    }
}
