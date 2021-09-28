package uk.ac.cam.cares.jps.agent.flood;

import java.time.Instant;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;

import org.apache.http.HttpHeaders;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
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
	    
    public static void main(String[] args) {
    	// this retrieves the high level information and uploads it to Blazegraph
    	initFloodStationsWithAPI();
    	
    	// create a table for each measure uploaded to Blazegraph
    	initTimeSeriesTables();
    	
    	// add rdf:type and coordinates in blazegraph format
    	addCoordinatesAndType();
    }
    
	/** 
	 * gets RDF data from the gov API and upload data as it is to Blazegraph
	 * The RDF data contains high level information for each station, e.g. 
	 * its location, what it measures
	 */
	static void initFloodStationsWithAPI() {
		Config.initProperties();
		try {
			// get rdf data from gov website
			LOGGER.info("Downloading station data from API");
			HttpGet request = new HttpGet("http://environment.data.gov.uk/flood-monitoring/id/stations.rdf");
			
			CloseableHttpClient httpclient = HttpClients.createDefault();
	        CloseableHttpResponse response = httpclient.execute(request);
	        LOGGER.info("Download complete");

	        // use Blazegraph's REST API to upload RDF data to a SPARQL endpoint
	        LOGGER.info("Posting data to Blazegraph");
	        
	        // tried a few methods to add credentials, this seems to be the only way that works
	        // i.e. setting it manually in the header
	        String auth = Config.kguser + ":" + Config.kgpassword;
	        String encoded_auth = Base64.getEncoder().encodeToString(auth.getBytes()); 
	        HttpPost postRequest = new HttpPost(Config.kgurl);
	        postRequest.setHeader(HttpHeaders.AUTHORIZATION, "Basic " + encoded_auth);
	        
	        // add contents downloaded from the API to the post request 
	        postRequest.setEntity(response.getEntity());
	        // then send the post request
	        httpclient.execute(postRequest);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * create a table for each measure
	 */
	static void initTimeSeriesTables() {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl, Config.kguser, Config.kgpassword);
		FloodSparql sparqlClient = new FloodSparql(storeClient);
		TimeSeriesClient<Instant> tsClient = 
				new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
		
		List<String> measures = sparqlClient.getMeasures();
		
		for (String measure : measures) {
			tsClient.initTimeSeries(Arrays.asList(measure), Arrays.asList(Double.class), null);
		}
	}
	
	/**
	 * Add rdf:type and coordinates in Blazegraph format
	 */
	static void addCoordinatesAndType() {
		// obtain stations added to blazegraph
		Config.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl, Config.kguser, Config.kgpassword);
		FloodSparql sparqlClient = new FloodSparql(storeClient);
        List<String> stations = sparqlClient.getStations();
        
        // add RDF:type to each station instance
		sparqlClient.addStationRdfType(stations);
		
		// add coordinates required by blazegraph geospatial support
		sparqlClient.addBlazegraphCoordinates(stations);
	}
}
