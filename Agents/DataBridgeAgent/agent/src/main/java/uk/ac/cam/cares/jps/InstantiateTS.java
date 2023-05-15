package uk.ac.cam.cares.jps;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;

import java.util.*;
import org.jooq.exception.DataAccessException;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;

import net.sf.jsqlparser.statement.select.Offset;

import java.sql.Connection;
import java.time.OffsetDateTime;
import java.text.SimpleDateFormat;
import java.time.*;
import java.util.TimeZone;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class InstantiateTS {
    
    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    TimeSeriesClient<OffsetDateTime> client;
    JSONObject values;
    JSONArray timestamp;
    Connection rdbConn;
    /**
     * The Zone offset of the timestamp (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZoneOffset.html)
     */
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;
    /**
    * The time unit used for all time series maintained by the ThingsBoard agent
    */
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();
    Type timeType;

    /**
     * The JSON key for the timestamp
     */
    public static final String timestampKey = "ts";

    public InstantiateTS(Connection rdbConnection,TripleStoreClientInterface kbClient, String timeClass) {
        rdbConn = rdbConnection;
        client = new TimeSeriesClient(kbClient, OffsetDateTime.class);

        for (String iri : values.keySet()){
            if (timestamp.length() != values.getJSONArray(iri).length()){
                throw new JPSRuntimeException("Timestamp size does not match data length for IRI:" + iri);
            }
        }

        if (timeClass.equals("AVERAGE")){
            timeType = Type.AVERAGE;
        }
        else if (timeClass.equals("STEPWISECUMULATIVE")){
            timeType = Type.STEPWISECUMULATIVE;
        }
        else if (timeClass.equals("CUMULATIVETOTAL")){
            timeType = Type.CUMULATIVETOTAL;
        }
        else if (timeClass.equals("INSTANTANEOUS")){
            timeType = Type.INSTANTANEOUS;
        }
        else if (timeClass.equals("GENERAL")){
            timeType = Type.GENERAL;
        }
        else{
            throw new JPSRuntimeException("Type of timeseries is not allowed. Choose from: Type.AVERAGE, Type.INSTANTANEOUS, Type.STEPWISECUMULATIVE, Type.CUMULATIVETOTAL");
        }

        
        
    }
        /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);
        // Then add the zone id
        return OffsetDateTime.of(localTime, ZONE_OFFSET);
    }

    /**
     * Converts the readings in form of maps to time series' using the mappings from JSON key to IRI.
     * @param ElectricalTemperatureHumidityReadings The readings as map.
     * @param TimestampReadings The timestamps as map.
     * @return A list of time series objects (one per mapping) that can be used with the time series client.
     */
    private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(JSONObject data, JSONArray timesArray) {
            List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
            List<String> iris = new ArrayList<>();
            List<List<?>> values = new ArrayList<>();
            
            List<OffsetDateTime> times = new ArrayList<>();
            for (int i = 0; i < timesArray.length(); i++){
                String tsString = timesArray.getString(i);
                times.add(convertStringToOffsetDateTime(tsString));
            }

            for (String iri : data.keySet()) {
                iris.add(iri);
                JSONArray valueArray = data.getJSONArray("values");
                List<Object> row = new ArrayList<>();
                for (int j =0; j < valueArray.length(); j++) {
                    //Defaults class to String, convert to respective needed data type when TS data is used
                    Object value = valueArray.get(j);

                    if (value instanceof Integer || value instanceof Long) {
                        long longValue = ((Number)value).longValue();
                        row.add(longValue);
                    } else if (value instanceof Boolean) {
                        boolean boolValue = ((Boolean)value).booleanValue();
                        row.add(boolValue);
                    } else if (value instanceof Float || value instanceof Double) {
                        double doubleValue = ((Number)value).doubleValue();
                        row.add(doubleValue);
                    } else if (JSONObject.NULL.equals(value)) {
                        Object nullValue = null;
                        row.add(nullValue);
                    } else {
                        String stringValue = valueArray.getString(j);
                        row.add(stringValue);
                    }
                }
                values.add(row);
            }
            

            TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
            timeSeries.add(currentTimeSeries);
        
        
        return timeSeries;
    }



    

    /**
     * Returns the class (datatype) corresponding to a JSON key.
     * @param jsonKey The JSON key as string.
     * @return The corresponding class as Class<?> object.
     */
    private Class<?> getClassFromJSONKey(String jsonKey) {
    	
    	Class<?> a = null;
        if (jsonKey.contains(timestampKey)) {
        	a = String.class;
        }
     return a;   
    }

    /**
     * Initializes all time series maintained by the agent (represented by the key to IRI mappings) if they do no exist
     * using the time series client.
     */
    public void initializeTimeSeriesIfNotExist() {
        List <String> iris = new ArrayList<>();
        values.keys().forEachRemaining(iris::add); 
        // Check whether IRIs have a time series linked and if not initialize the corresponding time series
        if(!timeSeriesExist(iris)) {
            // Get the classes (datatype) corresponding to each JSON key needed for initialization
            List<Class<?>> classes = iris.stream().map(this::getClassFromJSONKey).collect(Collectors.toList());
            // Initialize the time series
            try {
            client.initTimeSeries(iris, classes, timeUnit, rdbConn, timeType, null, null);
            LOGGER.info(String.format("Initialized time series with the following IRIs: %s", String.join(", ", iris)));
        } catch (Exception e) {
            throw new JPSRuntimeException("Could not initialize timeseries!");
        }
        }
            
    }


    /**
     * Updates the database with new readings.
     * @param RFIDVariableReadings The readings received from the RFID API
     */
    public void updateData(JSONObject data)throws IllegalArgumentException {
        // Transform readings in hashmap containing a list of objects for each JSON key,
        // will be empty if the JSON Object is empty
        /*
         * The format for the JSONObject from req param:
         * {
         *  "time" : [ts1, ts2, ...],
         *  "values" : { "IRI1" : [data1, data2, ...]}
         * }
         * 
         */
        timestamp = data.getJSONArray(timestampKey);
        values = data.getJSONObject("values");


        
        // Only do something if all readings contain data
        if(!values.isEmpty() && !timestamp.isEmpty()) {
            List<TimeSeries<OffsetDateTime>> timeSeries;
          
            timeSeries = convertReadingsToTimeSeries(values, timestamp);
            
            // Update each time series
            for (TimeSeries<OffsetDateTime> ts : timeSeries) {
                try {
                // Only update if there actually is data
                if (!ts.getTimes().isEmpty()) {
                    client.addTimeSeriesData(ts);
                    LOGGER.debug(String.format("Time series updated for following IRIs: %s", String.join(", ", ts.getDataIRIs())));
                }
                } catch (Exception e) {
                	throw new JPSRuntimeException ("Could not add timeseries data!");
                }
              
            }
        }
        // Is a problem as time series objects must be the same every time to ensure proper insert into the database
        else {
            throw new IllegalArgumentException("Readings can not be empty!");
        	}
    }


    /**
     * Checks whether a time series exists by checking whether any of the IRIs
     * that should be attached to the time series has no attachment using the time series client.
     * @param iris The IRIs that should be attached to the same time series provided as list of strings.
     * @return True if all IRIs have a time series attached, false otherwise.
     */
    private boolean timeSeriesExist(List<String> iris) {
        // If any of the IRIs does not have a time series the time series does not exist
        for(String iri: iris) {
        	try {
	            if (!client.checkDataHasTimeSeries(iri)) {
	                return false;
	            }
	        // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
        	} catch (DataAccessException e) {
        		if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
        			return false;
        		}
        		else {
        			throw e;
        		}        		
        	}
        }
        return true;
    }
    
}
