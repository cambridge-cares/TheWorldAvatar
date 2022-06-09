package uk.ac.cam.cares.jps.agent.flood;

import java.io.File;
import java.nio.file.Files;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Paths;
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
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.flood.objects.Station;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Downloads high level station information from http://environment.data.gov.uk/flood-monitoring/id/stations?_view=full.rdf
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
			InitialiseStations.api.setParameter("_view", "full");
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
        List<Station> stations = sparqlClient.getStationsOriginal();
        
        // add triples for OntoEMS
		sparqlClient.addStationTypeAndCoordinates(stations);
		sparqlClient.replaceMeasures(stations);
		
		// set to false by default, download it once and save it locally
		// takes a while to download because there is no API to download everything in 1 go
		if (Config.DOWNLOAD_DATUM) {
			JSONObject datumMap = sparqlClient.downloadDatum(stations);
			File file = new File(Config.DATUM_FILE);
			writeToFile(file, datumMap.toString(4));
		}

		// add datum triples for OntoEMS
		// File specified by DATUM_FILE needs to exist
		try {
			JSONObject datum_json = new JSONObject(Files.readString(Paths.get(Config.DATUM_FILE)));
			sparqlClient.addDatum(datum_json);
		} catch (JSONException | IOException e) {
			LOGGER.error("Failed to read datum file");
			LOGGER.error(e.getMessage());
		}
		
		// create a table for each measure uploaded to Blazegraph and a table to record last updated date
    	initTimeSeriesTables(tsClient, stations);
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
	static void initTimeSeriesTables(TimeSeriesClient<Instant> tsClient, List<Station> stations) {
		LOGGER.info("Initialising time series tables");
		
		List<String> measures = new ArrayList<>();
		for (Station station : stations) {
			for (String measure : station.getMeasures()){
				measures.add(measure);
			}
		}
		List<List<String>> ts_list = new ArrayList<>(measures.size());
		List<List<Class<?>>> classes = new ArrayList<>(measures.size());
		
		for (int i = 0; i < measures.size(); i++) {
			ts_list.add(Arrays.asList(measures.get(i)));
			classes.add(Arrays.asList(Double.class));
		}

		// table to record last updated time
		ts_list.add(Arrays.asList(Config.TIME_IRI));
		classes.add(Arrays.asList(LocalDate.class));
		
		tsClient.bulkInitTimeSeries(ts_list, classes, null);
		tsClient.disconnectRDB();
	}

	private static void writeToFile(File file, String output) {
		try {
			file.createNewFile();
			FileOutputStream outputStream = new FileOutputStream(file);
			byte[] strToBytes = output.getBytes();
			outputStream.write(strToBytes);
			outputStream.close();
			LOGGER.info("Created " + file.getAbsolutePath());
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
}
