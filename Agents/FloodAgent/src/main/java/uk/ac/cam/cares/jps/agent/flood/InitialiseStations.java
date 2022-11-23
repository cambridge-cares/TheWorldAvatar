package uk.ac.cam.cares.jps.agent.flood;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.agent.flood.objects.Measure;
import uk.ac.cam.cares.jps.agent.flood.objects.Station;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Downloads high level station information from http://environment.data.gov.uk/flood-monitoring/id/stations?_view=full.rdf
 * and post it to Blazegraph. The endpoint and credentials are specified via environment variables
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
    
    public static void main(String[] args) throws IOException, URISyntaxException {
		EndpointConfig endpointConfig = new EndpointConfig();
    	
    	// if these are null, they are in deployed mode, otherwise they should
    	// be set with mocks using their respective setters
    	if (api == null) {
    		InitialiseStations.api = new APIConnector("http://environment.data.gov.uk/flood-monitoring/id/stations.rdf");
			InitialiseStations.api.setParameter("_view", "full");
    	}
    	if (storeClient == null) {
    		InitialiseStations.storeClient = new RemoteStoreClient(endpointConfig.getKgurl(),endpointConfig.getKgurl(), endpointConfig.getKguser(), endpointConfig.getKgpassword());
    	}
    	if (sparqlClient == null) {
    		InitialiseStations.sparqlClient = new FloodSparql(storeClient);
    	}
    	if (tsClient == null) {
    		InitialiseStations.tsClient = new TimeSeriesClient<>(storeClient, Instant.class, endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
    	}
    	
    	// this retrieves the high level information and uploads it to Blazegraph
    	initFloodStationsWithAPI(InitialiseStations.api, InitialiseStations.storeClient);
    	
    	// obtain stations added to blazegraph
        List<Station> stations = sparqlClient.getStationsOriginal();
        
        // add triples for OntoEMS, also update stations objects
		sparqlClient.addMeasuresConcepts(stations);
		sparqlClient.addStationTypeAndCoordinates(stations);

		LOGGER.info("Creating layer in Geoserver");
		GeoServerClient geoserverclient = new GeoServerClient();
		geoserverclient.createWorkspace(EnvConfig.GEOSERVER_WORKSPACE);
		geoserverclient.createPostGISLayer(null, EnvConfig.GEOSERVER_WORKSPACE, EnvConfig.DATABASE, EnvConfig.LAYERNAME, new GeoServerVectorSettings());
		
		Path obdaFile = Path.of(EnvConfig.ONTOP_FILE);
		new OntopClient().updateOBDA(obdaFile);
		
		// set to false by default, download it once and save it locally
		// takes a while to download because there is no API to download everything in 1 go
		if (EnvConfig.DOWNLOAD_DATUM) {
			JSONObject datumMap = sparqlClient.downloadDatum(stations);
			File file = new File(EnvConfig.DATUM_FILE);
			writeToFile(file, datumMap.toString(4));
		}

		// add datum triples for OntoEMS
		// File specified by DATUM_FILE needs to exist
		try {
			if (EnvConfig.DATUM_FILE != null) {
				JSONObject datumJson = new JSONObject(Files.readString(Paths.get(EnvConfig.DATUM_FILE)));
				sparqlClient.addDatum(datumJson);
			}
		} catch (JSONException | IOException e) {
			LOGGER.error("Failed to read datum file");
			LOGGER.error(e.getMessage());
		}
		
		// instantiate connections between stations, e.g. <station1> <hasDownstreamStation> <station2>
		if (EnvConfig.INSTANTIATE_CONNECTIONS) {
			File connectionsFile = new File(EnvConfig.CONNECTIONS_FILE);
			if (connectionsFile.exists()) {
				sparqlClient.addConnections(connectionsFile);
			} else {
				LOGGER.warn("File containing connections between stations does not exist");
			}
		}

		// create a table for each measure uploaded to Blazegraph and a table to record last updated date
    	initTimeSeriesTables(tsClient, stations);
    }
    
	/** 
	 * gets RDF data from the gov API and upload data as it is to Blazegraph
	 * The RDF data contains high level information for each station, e.g. 
	 * its location, what it measures
	 * @throws IOException
	 */
	static void initFloodStationsWithAPI(APIConnector api, RemoteStoreClient storeClient) throws URISyntaxException, IOException {
		CloseableHttpClient httpClient = HttpClients.createDefault();
		try {
			// get rdf data from gov website
			HttpEntity responseEntity = api.getData(httpClient);

			// use Blazegraph's REST API to upload RDF data to a SPARQL endpoint
	        LOGGER.info("Posting data to Blazegraph");
	        
	        HttpPost postRequest = new HttpPost(storeClient.getUpdateEndpoint());	        
	        // add contents downloaded from the API to the post request 
	        postRequest.setEntity(responseEntity);
	        // then send the post request
	        httpClient.execute(postRequest);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		} finally {
			httpClient.close();
		}
	}
	
	/**
	 * create a table for each measure
	 */
	static void initTimeSeriesTables(TimeSeriesClient<Instant> tsClient, List<Station> stations) {
		LOGGER.info("Initialising time series tables");
		
		List<String> measures = new ArrayList<>();
		for (Station station : stations) {
			for (Measure measure : station.getMeasures()){
				measures.add(measure.getIri());
			}
		}
		List<List<String>> tsList = new ArrayList<>(measures.size());
		List<List<Class<?>>> classes = new ArrayList<>(measures.size());
		
		for (int i = 0; i < measures.size(); i++) {
			tsList.add(Arrays.asList(measures.get(i)));
			classes.add(Arrays.asList(Double.class));
		}

		// table to record last updated time
		tsList.add(Arrays.asList(EnvConfig.TIME_IRI));
		classes.add(Arrays.asList(LocalDate.class));
		
		tsClient.bulkInitTimeSeries(tsList, classes, null);
	}

	private static void writeToFile(File file, String output) {
		try {
            if(file.createNewFile()) {
				FileOutputStream outputStream = new FileOutputStream(file);
				byte[] strToBytes = output.getBytes();
				outputStream.write(strToBytes);
				outputStream.close();
				LOGGER.info("Created {}", file.getAbsolutePath());
			} else {
				LOGGER.error("File not created {}", file);
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
}
