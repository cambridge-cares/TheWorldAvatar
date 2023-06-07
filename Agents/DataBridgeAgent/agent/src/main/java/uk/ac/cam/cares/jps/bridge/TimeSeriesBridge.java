package uk.ac.cam.cares.jps.bridge;

import uk.ac.cam.cares.jps.DataBridgeAgent;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.*;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;

import java.sql.Connection;
import java.time.OffsetDateTime;
import java.time.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A bridge to instantiate the time series into the SPARQL endpoint and PostgreSQL database.
 *
 * @author qhouyee, Michael-TeguhLaksana
 */
public class TimeSeriesBridge {

    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    private final TimeSeriesClient<OffsetDateTime> client;
    private final RemoteRDBStoreClient RDBClient;
    private final Type timeType;
    // The Zone offset of the timestamp (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZoneOffset.html)
    private static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;
    // The time unit used for all time series
    private static final String TIME_UNIT = OffsetDateTime.class.getSimpleName();
    private static final String TIMESTAMP_KEY = "timestamp";

    /**
     * Standard Constructor to set up the relevant client based on configs.
     *
     * @param config    An array containing the SPARQL and SQL credentials.
     * @param timeClass A parameter indicating the time series class.
     */
    public TimeSeriesBridge(String[] config, String timeClass) {
        // Set up a client to access the RDB
        this.RDBClient = new RemoteRDBStoreClient(config[0], config[1], config[2]);
        // Create a new time series client
        RemoteStoreClient kbClient = new RemoteStoreClient(config[3], config[3]);
        this.client = new TimeSeriesClient<>(kbClient, OffsetDateTime.class);
        switch (timeClass) {
            case "AVERAGE":
                this.timeType = Type.AVERAGE;
                break;
            case "STEPWISECUMULATIVE":
                this.timeType = Type.STEPWISECUMULATIVE;
                break;
            case "CUMULATIVETOTAL":
                this.timeType = Type.CUMULATIVETOTAL;
                break;
            case "INSTANTANEOUS":
                this.timeType = Type.INSTANTANEOUS;
                break;
            case "GENERAL":
                this.timeType = Type.GENERAL;
                break;
            default:
                throw new JPSRuntimeException("Type of timeseries is not allowed. Choose from: Type.AVERAGE, Type.INSTANTANEOUS, Type.STEPWISECUMULATIVE, Type.CUMULATIVETOTAL");
        }
    }

    /**
     * Instantiate the time series inputs into the SQL database and SPARQL endpoint.
     *
     * @param data The time series data to be instantiated.
     */
    public JSONObject instantiateTimeSeries(JSONObject data) {
        JSONObject response = new JSONObject();
        JSONArray timestamps = data.getJSONArray(TIMESTAMP_KEY);
        JSONObject values = data.getJSONObject("values");
        try {
            initialiseTimeSeriesIfNotExist(values);
            addTimeSeries(timestamps, values);
            LOGGER.info("Data updated with new readings.");
            response.accumulate("Result", "Data updated with new readings.");
        } catch (Exception e) {
            LOGGER.fatal("Failed to instantiate time series:" + e);
            throw new JPSRuntimeException("Failed to instantiate time series:" + e);
        }
        return response;
    }

    /**
     * Initialises all time series if they do not exist using the time series client.
     *
     * @param values A JSON Object containing the time series iri and values.
     */
    private void initialiseTimeSeriesIfNotExist(JSONObject values) {
        List<String> iris = new ArrayList<>(values.keySet());
        if (!timeSeriesExist(iris)) {
            // Get the classes (datatype) corresponding to each JSON key needed for initialisation
            LOGGER.debug("Time series does not exist. Initialising time series in the database...");
            List<Class<?>> classes = iris.stream()
                    .map(iri -> getClassFromJSONKey(iri, values))
                    .collect(Collectors.toList());
            try (Connection conn = RDBClient.getConnection()) {
                client.initTimeSeries(iris, classes, TIME_UNIT, conn, timeType, null, null);
                LOGGER.info(String.format("Initialised time series with the following IRIs: %s", String.join(", ", iris)));
            } catch (Exception e) {
                LOGGER.error("Could not initialise time series! " + e);
                throw new JPSRuntimeException("Could not initialise time series! " + e);
            }
        }
    }

    /**
     * Verifies the existence of a time series by checking if all the IRIs have already been instantiated.
     *
     * @param iris The time series IRIs as a list of strings.
     * @return true if all time series IRIs have been instantiated, false otherwise.
     */
    private boolean timeSeriesExist(List<String> iris) {
        LOGGER.debug("Verifying if time series exist...");
        for (String iri : iris) {
            try (Connection conn = RDBClient.getConnection()) {
                // If any of the IRIs does not have a time series the time series does not exist
                if (!client.checkDataHasTimeSeries(iri, conn)) {
                    return false;
                }
                // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
            } catch (Exception e) {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                    LOGGER.fatal("ERROR: central RDB lookup table (\"dbTable\") failed to be initialised");
                    return false;
                }
                LOGGER.fatal(e);
                throw new JPSRuntimeException(e);
            }
        }
        return true;
    }

    /**
     * Returns the class (datatype) corresponding to the time series data.
     *
     * @param dataIri The data IRI that acts as a JSON key for the map.
     * @return The corresponding class as Class<?> object.
     */
    private Class<?> getClassFromJSONKey(String dataIri, JSONObject values) {
        if (dataIri.contains(TIMESTAMP_KEY)) {
            return String.class;
        } else {
            Object valueObject = values.getJSONArray(dataIri).get(0);
            return getClassFromValues(valueObject);
        }
    }

    private Class<?> getClassFromValues(Object value) {
        if (value instanceof Integer || value instanceof Long) {
            return Long.class;
        } else if (value instanceof Boolean) {
            return Boolean.class;
        } else if (value instanceof Float || value instanceof Double) {
            return Double.class;
        } else if (JSONObject.NULL.equals(value)) {
            return null;
        } else {
            return String.class;
        }
    }

    /**
     * Add time series data into the database.
     *
     * @param timestamps An JSON array containing the time series' time stamps.
     * @param values     A JSON Object containing the time series iri and values.
     */
    private void addTimeSeries(JSONArray timestamps, JSONObject values) throws IllegalArgumentException {
        for (String iri : values.keySet()) {
            if (timestamps.length() != values.getJSONArray(iri).length()) {
                LOGGER.fatal("Number of time stamps does not match their values for :" + iri);
                throw new JPSRuntimeException("Number of time stamps does not match their values for :" + iri);
            }
        }
        List<TimeSeries<OffsetDateTime>> timeSeries = convertReadingsToTimeSeries(timestamps, values);
        // Update each time series
        for (TimeSeries<OffsetDateTime> ts : timeSeries) {
            try (Connection conn = RDBClient.getConnection()) {
                // Only update if there actually is data
                if (!ts.getTimes().isEmpty()) {
                    client.addTimeSeriesData(ts, conn);
                    LOGGER.debug(String.format("Time series updated for following IRIs: %s", String.join(", ", ts.getDataIRIs())));
                }
            } catch (Exception e) {
                throw new JPSRuntimeException("Could not add timeseries data!");
            }
        }
    }

    /**
     * Converts the readings in form of maps to time series' using the mappings from JSON key to IRI.
     *
     * @param timesArray An JSON array containing the time series' time stamps.
     * @param values     A JSON Object containing the time series iri and values.
     * @return A list of time series objects (one per mapping) that can be used with the time series client.
     */
    private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(JSONArray timesArray, JSONObject values) {
        // Convert timestamp to a list for time series client
        List<OffsetDateTime> times = new ArrayList<>();
        for (int i = 0; i < timesArray.length(); i++) {
            String tsString = timesArray.getString(i);
            times.add(convertStringToOffsetDateTime(tsString));
        }
        // Convert the values into their required format
        List<String> iris = new ArrayList<>();
        // Each time series data will be added as a different row
        List<List<?>> dataValues = new ArrayList<>();
        for (String iri : values.keySet()) {
            // Add each IRI into the list
            iris.add(iri);
            JSONArray valueArray = values.getJSONArray(iri);
            List<Object> currentRow = new ArrayList<>();
            for (int j = 0; j < valueArray.length(); j++) {
                // Defaults class to String, convert to respective needed data type when TS data is used
                Object value = valueArray.get(j);
                if (value instanceof Integer || value instanceof Long) {
                    long longValue = ((Number) value).longValue();
                    currentRow.add(longValue);
                } else if (value instanceof Boolean) {
                    boolean boolValue = (Boolean) value;
                    currentRow.add(boolValue);
                } else if (value instanceof Float || value instanceof Double) {
                    double doubleValue = ((Number) value).doubleValue();
                    currentRow.add(doubleValue);
                } else if (JSONObject.NULL.equals(value)) {
                    currentRow.add(null);
                } else {
                    String stringValue = valueArray.getString(j);
                    currentRow.add(stringValue);
                }
            }
            dataValues.add(currentRow);
        }
        TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, dataValues);
        List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
        timeSeries.add(currentTimeSeries);
        return timeSeries;
    }

    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     *
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);
        // Then add the zone id
        return OffsetDateTime.of(localTime, ZONE_OFFSET);
    }
}
