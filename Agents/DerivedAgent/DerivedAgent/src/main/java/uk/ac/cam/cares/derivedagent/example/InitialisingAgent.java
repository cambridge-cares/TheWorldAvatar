package uk.ac.cam.cares.derivedagent.example;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.UUID;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivedquantity.DerivedQuantityClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

@WebServlet(urlPatterns = {"/InitialisingAgent"})
public class InitialisingAgent extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private static String baseURL = "http://localhost:8080/DerivedAgent";
	
	private static String mintime_agent_iri = ExampleSparqlClient.namespace + "mintime_agent";
	private static String mintime_agent_url = baseURL + ExampleDerivedAgent.URL_MINTIME;
	
	private static String maxtime_agent_iri = ExampleSparqlClient.namespace + "maxtime_agent";
	private static String maxtime_agent_url = baseURL + ExampleDerivedAgent.URL_MAXTIME;
	
	private static String timeduration_agent_iri = ExampleSparqlClient.namespace + "timeduration_agent";
	private static String timeduration_agent_url = baseURL + ExampleDerivedAgent.URL_DURATION;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(InitialisingAgent.class);
    
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	if (ExampleConfig.props == null) {
    		ExampleConfig.initProperties();
    	}
    	RemoteStoreClient storeClient = new RemoteStoreClient(ExampleConfig.kgurl,ExampleConfig.kgurl,ExampleConfig.kguser,ExampleConfig.kgpassword);
    	ExampleSparqlClient sparqlClient = new ExampleSparqlClient(storeClient);
    	DerivedQuantityClient devClient = new DerivedQuantityClient(storeClient);
    	
    	// record the IRIs of the created instances to link them later
    	String input = sparqlClient.createInputData();
    	// attach timestamp to input
    	devClient.addTimeInstance(input);
    	createTimeSeries(input, storeClient);
    	LOGGER.info("created input " + input);
    	
    	String mintime = sparqlClient.createMinTime(0);
    	LOGGER.info("created min time " + mintime);
    	
    	String maxtime = sparqlClient.createMaxTime(0);
    	LOGGER.info("created max time " + maxtime);
    	
    	String timeduration = sparqlClient.createTimeDuration(0);
    	LOGGER.info("created time duration " + timeduration);

    	// create three derived quantities
    	String derived_mintime = devClient.createDerivedQuantity(Arrays.asList(mintime), mintime_agent_iri, mintime_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for min time " + derived_mintime);
    	
    	String derived_maxtime = devClient.createDerivedQuantity(Arrays.asList(maxtime), maxtime_agent_iri, maxtime_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for max time " + derived_maxtime);
    	
    	String derived_timeduration = devClient.createDerivedQuantity(Arrays.asList(timeduration), timeduration_agent_iri, timeduration_agent_url, Arrays.asList(mintime,maxtime));
    	LOGGER.info("created derived quantity for time duration " + derived_timeduration);
    	
    	// check all connections between the derived quantities
    	// as time duration is derived from min time and max time, they get checked too
    	// the validate method only traverse down, not up
    	LOGGER.info("Validating " + derived_timeduration);
    	if (devClient.validateDerived(derived_timeduration)) {
    		LOGGER.info("validation success");
    	} else {
    		LOGGER.error("ERROR: validation fail");
    	}

    	return requestParams;
    }
    
    private static void createTimeSeries(String input_iri, StoreClientInterface storeClient) {
    	// set up time series client..
    	TimeSeriesRDBClient<Integer> tsClient = new TimeSeriesRDBClient<Integer>(Integer.class);
    	tsClient.setRdbURL(ExampleConfig.dburl);
    	tsClient.setRdbUser(ExampleConfig.dbuser);
    	tsClient.setRdbPassword(ExampleConfig.dbpassword);
    	
    	TimeSeriesSparql tsSparql = new TimeSeriesSparql(storeClient);
    	
    	// create a time series instance
    	String tsIRI = "http://" + UUID.randomUUID().toString();
    	tsSparql.initTS(tsIRI, Arrays.asList(input_iri), ExampleConfig.dburl, null);
    	
    	tsClient.initCentralTable();
    	tsClient.initTimeSeriesTable(Arrays.asList(input_iri), Arrays.asList(Integer.class), tsIRI);
    	
    	// create a new time series object with random numbers
    	Random rand = new Random();
    	List<Integer> time_column = Arrays.asList(rand.nextInt(),rand.nextInt());
    	List<List<?>> values = new ArrayList<>();
    	List<Integer> value_column = Arrays.asList(rand.nextInt(),rand.nextInt());
    	values.add(value_column);
    	TimeSeries<Integer> ts = new TimeSeries<Integer>(time_column, Arrays.asList(input_iri), values);
    	
    	tsClient.addTimeSeriesData(ts);
    }
}
