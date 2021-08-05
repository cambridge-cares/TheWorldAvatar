package uk.ac.cam.cares.derivation.example;

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
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


@WebServlet(urlPatterns = {"/InitialisingAgent"})
public class InitialisingAgent extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
    // this URL is based on the docker image, make sure the URL is accessible from the derived quantity's calling entity
	private static String baseURL = "http://derivedagent:8080/DerivedAgent";
	
	private static String mintime_agent_iri = SparqlClient.namespace + "mintime_agent";
	private static String mintime_agent_url = baseURL + DerivationAgents.URL_MINTIME;
	
	private static String maxtime_agent_iri = SparqlClient.namespace + "maxtime_agent";
	private static String maxtime_agent_url = baseURL + DerivationAgents.URL_MAXTIME;
	
	private static String timeduration_agent_iri = SparqlClient.namespace + "timeduration_agent";
	private static String timeduration_agent_url = baseURL + DerivationAgents.URL_DURATION;
	
	private static String mintimecalc_agent_iri = SparqlClient.namespace + "mintimecalc_agent";
	private static String mintimecalc_agent_url = baseURL + DerivationAgents.URL_MINTIMECALC;
	
	private static String maxtimecalc_agent_iri = SparqlClient.namespace + "maxtimecalc_agent";
	private static String maxtimecalc_agent_url = baseURL + DerivationAgents.URL_MAXTIMECALC;
	
	private static final Logger LOGGER = LoggerFactory.getLogger(InitialisingAgent.class);
    
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
    	
    	String[] mintime = sparqlClient.createMinTime(0);
    	LOGGER.info("created min time " + mintime[0]);
    	InstancesDatabase.MinTime = mintime[0];
    	
    	String[] maxtime = sparqlClient.createMaxTime(0);
    	LOGGER.info("created max time " + maxtime[0]);
    	InstancesDatabase.MaxTime = maxtime[0];
    	
    	String[] timeduration = sparqlClient.createTimeDuration(0);
    	LOGGER.info("created time duration " + timeduration[0]);
    	InstancesDatabase.TimeDuration = timeduration[0];

    	String[] mintimecalc = sparqlClient.createMinTimeCalc(0);	
    	LOGGER.info("created min time calc " + mintimecalc[0]);
    	
    	String[] maxtimecalc = sparqlClient.createMaxTimeCalc(0);
    	LOGGER.info("created max time calc " + maxtimecalc[0]);
    	
    	// create three derived quantities
    	String derived_mintime = devClient.createDerivation(Arrays.asList(mintime), mintime_agent_iri, mintime_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for min time " + derived_mintime);
    	InstancesDatabase.DerivedQuantityMinTime = derived_mintime;
    	
    	String derived_maxtime = devClient.createDerivation(Arrays.asList(maxtime), maxtime_agent_iri, maxtime_agent_url, Arrays.asList(input));
    	LOGGER.info("created derived quantity for max time " + derived_maxtime);
    	InstancesDatabase.DerivedQuantityMaxTime = derived_maxtime;
    	
    	String derived_timeduration = devClient.createDerivation(Arrays.asList(timeduration), timeduration_agent_iri, timeduration_agent_url, Arrays.asList(mintime[0],maxtime[0]));
    	LOGGER.info("created derived quantity for time duration " + derived_timeduration);
    	InstancesDatabase.DerivedQuantityTimeDuration = derived_timeduration;
    	
    	String derived_mintimecalc = devClient.createDerivation(Arrays.asList(mintimecalc), mintimecalc_agent_iri, mintimecalc_agent_url, Arrays.asList(timeduration[0],maxtime[0]));
    	LOGGER.info("created derived quantity for min time derived " + derived_mintimecalc);
    	InstancesDatabase.DerivedQuantityMinTimeCalc = derived_mintimecalc;
    	
    	String derived_maxtimecalc = devClient.createDerivation(Arrays.asList(maxtimecalc), maxtimecalc_agent_iri, maxtimecalc_agent_url, Arrays.asList(timeduration[0],mintime[0]));
    	LOGGER.info("created derived quantity for max time calc " + derived_maxtimecalc);
    	InstancesDatabase.DerivedQuantityMaxTimeCalc = derived_maxtimecalc;
    	
    	// check all connections between the derived quantities
    	// as time duration is derived from min time and max time, they get checked too
    	// the validate method only traverse down, not up
    	LOGGER.info("Validating " + derived_timeduration);
    	if (devClient.validateDerivation(derived_timeduration)) {
    		LOGGER.info("validation success");
    	} else {
    		LOGGER.error("ERROR: validation fail");
    	}

    	return requestParams;
    }
    
	@Override
	public boolean validateInput(JSONObject requestParams) {
		return true;
	}
	
    private static void createTimeSeries(String input_iri, TimeSeriesClient<Integer> tsClient) {
    	tsClient.initTimeSeries(Arrays.asList(input_iri), Arrays.asList(Integer.class), null);
    	
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
