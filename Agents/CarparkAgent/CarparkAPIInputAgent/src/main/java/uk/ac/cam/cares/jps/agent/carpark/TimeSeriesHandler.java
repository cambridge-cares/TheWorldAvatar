package uk.ac.cam.cares.jps.agent.carpark;

import org.json.JSONArray;
import org.json.JSONObject;
import org.jooq.exception.DataAccessException;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.text.SimpleDateFormat;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TimeSeriesHandler {
    public static final Logger LOGGER = LogManager.getLogger(TimeSeriesHandler.class);
    private TimeSeriesClient<OffsetDateTime> tsclient;
    private final List<JSONKeyToIRIMapper> mappings;
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();
    public static final String timestampKey = "time";
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;

    /**
     * Constructor for TimeSeriesHandler
     * @param mappings mapping for keys to data IRIs
     * @throws IOException
     */
    public TimeSeriesHandler(List<JSONKeyToIRIMapper> mappings) throws IOException {
        this.mappings = mappings;
    }

    /**
     * retrieve the number of timeseries from mappings
     * @return number of timeseries
     */
    public int getNumberofTimeSeries() {
        return mappings.size();
    }

    /**
     * Set timeseries client
     * @param tsclient timeseries cient instance
     */
    public void setTsClient(TimeSeriesClient<OffsetDateTime> tsclient) {
        this.tsclient = tsclient;
    }

    /**
     * Check whether timeseries exist and instantiate the timeseries if it does not exist
     */
    public void initializeTimeSeriesIfNotExist() {
        for (JSONKeyToIRIMapper mapping : mappings) {
            List<String> iris = mapping.getAllIRIs();
            if (!timeSeriesExist(iris)) {
                List<Class<?>> classes = iris.stream().map(this::getClassFromJSONKey).collect(Collectors.toList());
                try {
                    tsclient.initTimeSeries(iris, classes, timeUnit);
                    LOGGER.info(() -> "Initialized time series with the following IRIs: " + String.join(", ", iris));
                } catch (Exception e) {
                    LOGGER.fatal("Could not instantiate TimeSeries ", e);
                    throw new JPSRuntimeException("Could not instantiate TimeSeries" + e);
                }
            }
        }
    }

    /**
     * CHeck whether timeseries exist
     * @param iris list of data IRIs
     * @return True or False
     */
    private boolean timeSeriesExist(List<String> iris) {
        for (String iri : iris) {
            try {
                if (!tsclient.checkDataHasTimeSeries(iri)) {
                    return false;
                }
            } catch (DataAccessException e) {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                    return false;
                } else {
                    throw e;
                }
            }
        }
        return true;
    }

    /**
     * Parse and upload timeseries data to the knowledge graph
     * @param carparkReadings carpark readings retrieved via API
     * @throws IllegalArgumentException
     */
    public void updateData(JSONObject carparkReadings) throws IllegalArgumentException {
        Map<String, List<?>> carparkReadingsMap = jsonObjectToMap(carparkReadings);

        if (!carparkReadings.isEmpty()) {
            List<TimeSeries<OffsetDateTime>> timeSeries;
            timeSeries = convertReadingsToTimeSeries(carparkReadingsMap);
            for (TimeSeries<OffsetDateTime> ts : timeSeries) {
                // Retrieve current maximum time to avoid duplicate entries (can be null if no data is in the database yet)
                OffsetDateTime endDataTime;
                List<String> dataIRIs = ts.getDataIRIs();
                try {
                    endDataTime = tsclient.getMaxTime(dataIRIs.get(0));
                } catch (Exception e) {
                    LOGGER.error("Could not get max time for {}. The current time series readings will be ignored. Proceeding to next set...", dataIRIs.get(0));
                    break;
                }
                OffsetDateTime startCurrentTime = ts.getTimes().get(0);
                // If there is already a maximum time
                if (endDataTime != null) {
                    // If the new data overlaps with existing timestamps, prune the new ones
                    if (startCurrentTime.isBefore(endDataTime))
                        ts = pruneTimeSeries(ts, endDataTime);
                }
                // Only update if there actually is data
                if (!ts.getTimes().isEmpty()) {
                    try {
                        tsclient.addTimeSeriesData(ts);
                        LOGGER.debug(() -> "Time series updated for following IRIs: " + String.join(", ", dataIRIs));
                    } catch (Exception e) {
                        throw new JPSRuntimeException("Could not add timeseries!");
                    }
                }
            }
        }
    }

    /**
     * Parse readings to map
     * @param carparkReadings carpark readings retrieved via API
     * @return Map containing keys and timeseries values
     */
    private Map<String, List<?>> jsonObjectToMap(JSONObject carparkReadings) {
        // First save the values as Object //
        Map<String, List<?>> readingsMap = new HashMap<>();
        Map<String, List<Object>> firstMap = new HashMap<>();
        JSONArray jsArr;
        try {
            jsArr = carparkReadings.getJSONArray("value");
            for (int i = 0; i < jsArr.length(); i++) {
                JSONObject currentEntry = jsArr.getJSONObject(i);

                String key1 = "AvailableLots";
                Object value = currentEntry.get(key1);
                String key = key1 + "_" + currentEntry.get("CarParkID") + "_" + currentEntry.get("LotType");
                firstMap.put(key, new ArrayList<>());

                firstMap.get(key).add(value);

                List<Object> valuesUntyped = firstMap.get(key);
                List<?> valuesTyped = valuesUntyped.stream().map(x -> ((Number) x).intValue()).collect(Collectors.toList());

                readingsMap.put(key, valuesTyped);
            }
            long timestamp = System.currentTimeMillis();
            Date date = new java.util.Date(timestamp);
            SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

            Object ts = sdf.format(date);

            String k = "time";
            firstMap.put(k, new ArrayList<>());
            firstMap.get(k).add(ts);

            List<Object> valueUntyped = firstMap.get(k);
            List<?> valueTyped = valueUntyped.stream().map(Object::toString).collect(Collectors.toList());

            readingsMap.put(k, valueTyped);
        } catch (Exception e) {
            LOGGER.fatal("Readings can not be empty!", e);
            throw new JPSRuntimeException("Readings can not be empty!", e);
        }
        return readingsMap;
    }

    /**
     * Convert mapped readings to timeseries
     * @param carparkReadingsMap map containing keys and timeseries values
     * @return list of timeseries
     */
    private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(Map<String, List<?>> carparkReadingsMap) {
        // Extract the timestamps by mapping the private conversion method on the list items
        // that are supposed to be string (toString() is necessary as the map contains lists of different types)
        List<OffsetDateTime> carparkTimestamps = carparkReadingsMap.get(TimeSeriesHandler.timestampKey).stream().map(timestamp -> (convertStringToOffsetDateTime(timestamp.toString()))).collect(Collectors.toList());
        try {
            // Construct a time series object for each mapping
            List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
            for (JSONKeyToIRIMapper mapping : mappings) {
                // Initialize the list of IRIs
                List<String> iris = new ArrayList<>();
                // Initialize the list of list of values
                List<List<?>> values = new ArrayList<>();
                List<Double> NaN = new ArrayList<>();
                NaN.add(Double.NaN);
                List<?> NaNTyped = NaN.stream().map(x -> ((Number) x).doubleValue()).collect(Collectors.toList());
                for (String key : mapping.getAllJSONKeys()) {
                    // Add IRI
                    iris.add(mapping.getIRI(key));
                    if (carparkReadingsMap.containsKey(key)) {
                        values.add(carparkReadingsMap.get(key));
                    } else {
                        values.add(NaNTyped);
                    }
                }
                List<OffsetDateTime> times = carparkTimestamps;
                // Create the time series object and add it to the list
                TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
                timeSeries.add(currentTimeSeries);
            }
            return timeSeries;
        } catch (Exception e) {
            LOGGER.fatal("Readings cannot be converted to Proper TimeSeries.", e);
            throw new JPSRuntimeException("Readings cannot be converted to Proper TimeSeries.", e);
        }
    }

    /**
     * Convert timestamp string to include offset
     * @param timestamp timestamp as string
     * @return converted timestamp with offset
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);
        // Then add the zone id
        return OffsetDateTime.of(localTime, ZONE_OFFSET);
    }

    /**
     * Prune timeseries
     * @param timeSeries timeseries object
     * @param timeThreshold threshold that determines when to start pruning
     * @return pruned timeseries
     */
    private TimeSeries<OffsetDateTime> pruneTimeSeries(TimeSeries<OffsetDateTime> timeSeries, OffsetDateTime timeThreshold) {
        // Find the index from which to start
        List<OffsetDateTime> times = timeSeries.getTimes();
        int index = 0;
        while (index < times.size()) {
            if (times.get(index).isAfter(timeThreshold)) {
                break;
            }
            index++;
        }
        // Prune timestamps
        List<OffsetDateTime> newTimes = new ArrayList<>();
        // There are timestamps above the threshold
        if (index != times.size()) {
            // Prune the times
            newTimes = new ArrayList<>(times.subList(index, times.size()));
        }
        // Prune data
        List<List<?>> newValues = new ArrayList<>();
        // Prune the values
        for (String iri : timeSeries.getDataIRIs()) {
            // There are timestamps above the threshold
            if (index != times.size()) {
                newValues.add(timeSeries.getValues(iri).subList(index, times.size()));
            } else {
                newValues.add(new ArrayList<>());
            }
        }
        return new TimeSeries<>(newTimes, timeSeries.getDataIRIs(), newValues);
    }

    private Class<?> getClassFromJSONKey(String jsonKey) {
        return Double.class;
    }
}
