package uk.ac.cam.cares.jps.agent.flood;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.ParseException;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import uk.ac.cam.cares.jps.agent.flood.objects.Measure;
import uk.ac.cam.cares.jps.agent.flood.objects.Station;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Downloads data for 1 day and uploads it to the PostgreSQL database
 * Input: date in the form "2021-09-20"
 * @author Kok Foong Lee
 *
 */
public class UpdateStations {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(UpdateStations.class);
    
    // err msg
    private static final String ARG_MISMATCH = "Only one date argument is allowed";
	// json key
	private static final String ITEMS = "items";
	
    // will be replaced with mocks in junit tests
    private static APIConnector api = null;
    private static FloodSparql sparqlClient = null;
    private static TimeSeriesClient<Instant> tsClient = null;

	private static RemoteRDBStoreClient remoteRDBStoreClient = null; 
    
    // setters to replace these with mocks
    public static void setAPIConnector(APIConnector api) {
    	UpdateStations.api = api;
    }
    public static void setSparqlClient(FloodSparql sparqlClient) {
    	UpdateStations.sparqlClient = sparqlClient;
    }
    public static void setTsClient(TimeSeriesClient<Instant> tsClient) {
    	UpdateStations.tsClient = tsClient;
    }
    
	public static void main(String[] args) {
		EndpointConfig endpointConfig = new EndpointConfig();
		LocalDate date;
		
		// input validation
		if (args.length != 1) {
			LOGGER.error(ARG_MISMATCH);
			throw new JPSRuntimeException(ARG_MISMATCH);
		}
		try {
			date = LocalDate.parse(args[0]);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}

		// if these are null, they are in deployed mode, otherwise they should
    	// be set with mocks using their respective setters
    	if (api == null) {
    		try {
				UpdateStations.api = new APIConnector("http://environment.data.gov.uk/flood-monitoring/data/readings");
			} catch (URISyntaxException e) {
				LOGGER.error(e.getMessage());
				throw new JPSRuntimeException(e);
			}
    	}
    	UpdateStations.api.setParameter("date", date.toString());
    	
    	if (sparqlClient == null) {
    		RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
    		UpdateStations.sparqlClient = new FloodSparql(storeClient);
    	}
    	if (tsClient == null) {
    		RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(), endpointConfig.getKgurl());
    		UpdateStations.tsClient = new TimeSeriesClient<>(storeClient, Instant.class, endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
    	}

		if (remoteRDBStoreClient == null) {
			remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(), endpointConfig.getDbuser(), endpointConfig.getDbpassword());
		}
    	
		LOGGER.info("Updating data for {}", date);
		
		ProcessedData processedData;
		try {
			processedData = processAPIResponse(api);
		} catch (ParseException | IOException | URISyntaxException e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}

		// upload to postgres
		try (Connection conn = remoteRDBStoreClient.getConnection()) {
			uploadDataToRDB(tsClient, sparqlClient, processedData, conn);

			List<String> measureIRIs = processedData.getDataIriList();
			
			// checks final value and marks it as normal/high/low
			List<Measure> stageScaleMeasureList = sparqlClient.addRangeForStageScale(tsClient, measureIRIs, conn);
			List<Measure> downstageMeasureList = sparqlClient.addRangeForDownstageScale(tsClient, measureIRIs, conn);

			// calculate difference between first and final values, and mark as rising/falling/steady
			Instant lowerbound = date.atStartOfDay(ZoneOffset.UTC).toInstant();
			Instant upperbound = date.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant().minusSeconds(1);
			sparqlClient.addTrends(tsClient, stageScaleMeasureList, lowerbound, upperbound, conn);
			sparqlClient.addTrends(tsClient, downstageMeasureList, lowerbound, upperbound, conn);

			// update last updated date
			addUpdateDate(date, conn);

			// clean up
			deleteEmptyTables(conn);
		} catch (SQLException e) {
			LOGGER.error("Failed to close connection to RDB");
			LOGGER.error(e.getMessage());
		}
	}
	
	/**
	 * puts data into datatime_map and datavalue_map
	 * first element is datatime_map, second element is datavalue_map
	 * @param response
	 * @throws IOException 
	 * @throws ParseException 
	 * @throws URISyntaxException 
	 */
	static ProcessedData processAPIResponse(APIConnector api) throws ParseException, IOException, URISyntaxException {
		LOGGER.info("Processing data from API");
		File readingsFile = Paths.get(EnvConfig.READINGS_DIR, "readings.json").toFile();
		CloseableHttpClient httpClient = HttpClients.createDefault();
		HttpEntity response = api.getData(httpClient);
		// write data to file (easier to debug)
		FileOutputStream outputStream = new FileOutputStream(readingsFile);
		response.writeTo(outputStream);
		httpClient.close();

		// read data from downloaded file
		FileInputStream inputStream = new FileInputStream(readingsFile);
		JSONTokener tokener = new JSONTokener(inputStream);
		JSONObject responseJo = new JSONObject(tokener);
        JSONArray readings = responseJo.getJSONArray(ITEMS);
        
        // collect data belonging to the same URL into lists
        // this reduces the number of uploads required
		ProcessedData processedData = new ProcessedData();
        String dataIRI = null;
        int numFail = 0;
        for (int i = 0; i < readings.length(); i++) {
        	try {
	        	dataIRI = readings.getJSONObject(i).getString("measure");
	        	
	        	// if it is a JSON Array, take the average
	        	// not clear why more than 1 value is given
	        	Double value = readings.getJSONObject(i).optDouble("value");
				if (value == Double.NaN) {
					value = readings.getJSONObject(i).getJSONArray("value").toList().stream().mapToDouble(double.class::cast).average().getAsDouble();
	        		LOGGER.info("More than 1 value is given for a data point");
	        		LOGGER.info(readings.getJSONObject(i));
	        		LOGGER.info("Taking the average");
				}
	        	
	        	Instant timestamp = Instant.parse(readings.getJSONObject(i).getString("dateTime"));
	        	processedData.addData(dataIRI,timestamp,value);
        	} catch (Exception e) {
        		numFail += 1;
        		LOGGER.warn(readings.getJSONObject(i));
        		LOGGER.warn(e.getMessage());
        	}
        }
        
        LOGGER.info("Received a total of {} readings", readings.length());
        LOGGER.info("Organised into {} groups", processedData.getSize());
        LOGGER.info("Failed to process {} readings", numFail);
        
        return processedData;
	}
	
	static void uploadDataToRDB(TimeSeriesClient<Instant> tsClient, FloodSparql sparqlClient,
			ProcessedData processedData, Connection conn) {
        Iterator<String> iter = processedData.getDataIriIterator();
        int numFailures = 0;
        
        LOGGER.info("Uploading data to postgres");
        while (iter.hasNext()) {
        	String dataIRI = iter.next();
        	
        	// try to initialise table if it does not exist
        	if (!tsClient.checkDataHasTimeSeries(dataIRI, conn)) {
        		LOGGER.info("{} is not present in the initial rdf data", dataIRI);
    			LOGGER.info("Attempting to initialise <{}>", dataIRI);
    			
				try {
					// Obtain station name for this measure
					CloseableHttpClient httpClient = HttpClients.createDefault();
					HttpEntity response = new APIConnector(dataIRI).getData(httpClient);
					JSONObject responseJo = new JSONObject(EntityUtils.toString(response));
					
					// get the station that measures this quantity
					JSONObject items = responseJo.getJSONObject(ITEMS);
					String station = items.getString("station");
					String unit = items.getString("unitName");
					String parameterName = items.getString("parameterName");
					String qualifier = items.getString("qualifier");
					
					// check if station exists, if not, instantiate
					if (!sparqlClient.checkStationExists(station)) {
						HttpEntity newstation = new APIConnector(station).getData(httpClient);
						JSONObject newstationJo = new JSONObject(EntityUtils.toString(newstation));
						if (newstationJo.getJSONObject(ITEMS).has("lat")) {
							LOGGER.info("Instantiating a new station {}", station);
							sparqlClient.postToRemoteStore(new APIConnector(station+".rdf").getData(httpClient));

							// add approripate rdf type and blazegraph coordinates
							Station stationObject = new Station(station);
							stationObject.setLat(newstationJo.getJSONObject(ITEMS).getDouble("lat"));
							stationObject.setLon(newstationJo.getJSONObject(ITEMS).getDouble("lon"));
							Measure measure = new Measure(dataIRI);
							measure.setParameterName(parameterName);
							stationObject.addMeasure(measure);

							sparqlClient.addStationTypeAndCoordinates(Arrays.asList(stationObject));
						}
					}

					// add this missing information in blazegraph and rdb
					sparqlClient.addMeasureToStation(station, dataIRI,unit,parameterName,qualifier);
					
					tsClient.initTimeSeries(Arrays.asList(dataIRI), Arrays.asList(Double.class), null, conn);
					
					LOGGER.info("Created new table successfully");
				} catch (Exception e1) {
					numFailures += 1;
					LOGGER.error(e1.getMessage());
					LOGGER.error("Failed to initialise <{}>", dataIRI);
					continue;
				} 
        	}
        	
        	try {
    			// create time series object to upload to the client
                List<List<?>> values = new ArrayList<>();
                values.add(processedData.getValues(dataIRI));
                TimeSeries<Instant> ts = new TimeSeries<>(processedData.getTimestamps(dataIRI), Arrays.asList(dataIRI), values);
                tsClient.addTimeSeriesData(ts, conn);
                LOGGER.debug("Uploaded data for {}", dataIRI);
        	} catch (Exception e) {
        		numFailures += 1;
        	    LOGGER.error(e.getMessage());
        	    LOGGER.error("Failed to upload time series for {}", dataIRI);
        	}
        }
        
        LOGGER.info("Failed to add {} data set out of the processed data", numFailures);
	}

	static void addUpdateDate(LocalDate date, Connection conn) {
		List<List<?>> values = new ArrayList<>();
		values.add(Arrays.asList(date));
		TimeSeries<Instant> ts = new TimeSeries<>(Arrays.asList(date.atStartOfDay(ZoneOffset.UTC).toInstant()), Arrays.asList(EnvConfig.TIME_IRI), values);
		tsClient.addTimeSeriesData(ts, conn);
	}

	static void deleteEmptyTables(Connection conn) {
		List<String> measures = sparqlClient.getAllMeasuresWithTimeseries();

		List<String> emptyMeasures = new ArrayList<>();
		for (String measure : measures) {
			if(tsClient.getLatestData(measure, conn).getTimes().isEmpty()) {
				tsClient.deleteIndividualTimeSeries(measure, conn);
				emptyMeasures.add(measure);
			}
		}

		sparqlClient.deleteMeasures(emptyMeasures);
	}
}
