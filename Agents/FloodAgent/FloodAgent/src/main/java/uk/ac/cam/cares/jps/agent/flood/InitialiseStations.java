package uk.ac.cam.cares.jps.agent.flood;

import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Downloads high level station information from http://environment.data.gov.uk/flood-monitoring/id/stations.rdf
 * and post it to Blazegraph. The endpoint and credentials need to be saved at
 * credentials.properties
 * @author Kok Foong Lee
 *
 */
public class InitialiseStations {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(InitialiseStations.class);
    
    // will be replaced with mocks in junit tests
    private static APIConnector api = null;
    private static FloodSparql sparqlClient = null;
    private static TimeSeriesClient<Instant> tsClient = null;
    private static RemoteStoreClient storeClient = null;
	
    // setters to replace these with mocks
    public static void setAPIConnector(APIConnector api) {
    	InitialiseStations.api = api;
    }
    public static void setSparqlClient(FloodSparql sparqlClient) {
    	InitialiseStations.sparqlClient = sparqlClient;
    }
    public static void setTsClient(TimeSeriesClient<Instant> tsClient) {
    	InitialiseStations.tsClient = tsClient;
    }
    public static void setStoreClient(RemoteStoreClient storeClient) {
    	InitialiseStations.storeClient = storeClient;
    }
    
    public static void main(String[] args) {
    	Config.initProperties();
    	
    	// if these are null, they are in deployed mode, otherwise they should
    	// be set with mocks using their respective setters
    	if (api == null) {
    		InitialiseStations.api = new APIConnector("http://environment.data.gov.uk/flood-monitoring/id/stations.rdf");
    	}
    	if (storeClient == null) {
    		InitialiseStations.storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl, Config.kguser, Config.kgpassword);
    	}
    	if (sparqlClient == null) {
    		InitialiseStations.sparqlClient = new FloodSparql(storeClient);
    	}
    	if (tsClient == null) {
    		InitialiseStations.tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
    	}
    	
    	// this retrieves the high level information and uploads it to Blazegraph
    	initFloodStationsWithAPI(InitialiseStations.api, InitialiseStations.storeClient);
    	
    	// obtain stations added to blazegraph
        List<String> stations = sparqlClient.getStations();
        
        // add RDF:type to each station instance
		sparqlClient.addStationRdfType(stations);
		
		// add coordinates required by blazegraph geospatial support
		sparqlClient.addBlazegraphCoordinatesAndVisID();
		
		// create a table for each measure uploaded to Blazegraph
    	initTimeSeriesTables(sparqlClient, tsClient);
		
		// add last updated date, an arbitrary date 100 years ago
		sparqlClient.addUpdateDate(LocalDate.now().minusYears(100));
    }
    
	/** 
	 * gets RDF data from the gov API and upload data as it is to Blazegraph
	 * The RDF data contains high level information for each station, e.g. 
	 * its location, what it measures
	 */
	static void initFloodStationsWithAPI(APIConnector api, RemoteStoreClient storeClient) {
		try {
			// get rdf data from gov website
			HttpEntity response_entity = api.getData();
			
	        // use Blazegraph's REST API to upload RDF data to a SPARQL endpoint
	        LOGGER.info("Posting data to Blazegraph");
	        
	        // tried a few methods to add credentials, this seems to be the only way that works
	        // i.e. setting it manually in the header
	        String auth = storeClient.getUser() + ":" + storeClient.getPassword();
	        String encoded_auth = Base64.getEncoder().encodeToString(auth.getBytes()); 
	        HttpPost postRequest = new HttpPost(storeClient.getUpdateEndpoint());
	        postRequest.setHeader(HttpHeaders.AUTHORIZATION, "Basic " + encoded_auth);
	        
	        // add contents downloaded from the API to the post request 
	        postRequest.setEntity(response_entity);
	        // then send the post request
	        CloseableHttpClient httpclient = HttpClients.createDefault();
	        httpclient.execute(postRequest);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * create a table for each measure
	 */
	static void initTimeSeriesTables(FloodSparql sparqlClient, TimeSeriesClient<Instant> tsClient) {
		LOGGER.info("Initialising time series tables");
		
		List<String> measures = sparqlClient.getMeasures();
		List<List<String>> ts_list = new ArrayList<>(measures.size());
		List<List<Class<?>>> classes = new ArrayList<>(measures.size());
		
		for (int i = 0; i < measures.size(); i++) {
			ts_list.add(Arrays.asList(measures.get(i)));
			classes.add(Arrays.asList(Double.class));
		}
		
		tsClient.bulkInitTimeSeries(ts_list, classes, null);
		tsClient.disconnectRDB();
	}
}
