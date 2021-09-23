package uk.ac.cam.cares.jps.agent.flood;

import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;

import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.BufferedHttpEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class InitialiseStations {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(InitialiseStations.class);
	    
    public static void main(String[] args) {
    	// this retrieves the high level information and uploads it to Blazegraph
    	initFloodStationsWithAPI();
    	
    	// create a table for each measure uploaded to Blazegraph
    	initTimeSeriesTables();
    	
    	// 
    	addCoordinatesAndType();
    }
    
	/** 
	 * gets RDF data from the gov API and upload data as it is to Blazegraph
	 * The RDF data contains high level information for each station, e.g. 
	 * its location, what it measures
	 * @throws URISyntaxException
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	static void initFloodStationsWithAPI() {
		Config.initProperties();
		try {
			// get rdf data from gov website
			HttpGet request = new HttpGet("http://environment.data.gov.uk/flood-monitoring/id/stations.ttl");
			
			CloseableHttpClient httpclient = HttpClients.createDefault();
	        CloseableHttpResponse response = httpclient.execute(request);

	        // use Blazegraph's REST API to upload RDF data to a SPARQL endpoint
	        URIBuilder endpoint = new URIBuilder(Config.kgurl);
	        endpoint.setUserInfo(Config.kguser, Config.kgpassword);
	        HttpPost postRequest = new HttpPost(endpoint.build());
	        
            // solution from https://stackoverflow.com/questions/8256550/cannot-retry-request-with-a-non-repeatable-request-entity
	        postRequest.setEntity(new BufferedHttpEntity(response.getEntity()));
	        response = httpclient.execute(postRequest);
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
	
	static void deleteTimeSeriesData() {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl);
		TimeSeriesClient<Instant> tsClient = 
				new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
		tsClient.deleteAll();
	}
	
	/**
	 * not complete
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
