package com.cmclinnovations.featureinfo.kg;

import java.io.IOException;
import java.sql.Connection;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This class handles querying a Blazegraph endpoint to determine what measurements
 * are available, then retrieving these from the PostGreSQL endpoint.
 */
public class TimeHandler {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(TimeHandler.class);

    /**
     * Constant name for measurement IRI column
     */
    private static final String MEASUREMENT = "Measurement";
    
    /**
     * Constant name for forecast IRI column
     */
    private static final String FORECAST = "Forecast";

    /**
     * IRI of the asset.
     */
    private final String iri;

    /**
     * Name of matching class.
     */
    private final String classMatch;

    /**
     * Endpoint(s) for the KG.
     */
    private final List<ConfigEndpoint> endpoints = new ArrayList<>();

    /**
     * Connection to KG.
     */
    private RemoteStoreClient rsClient;

    /**
     * Connection to RDB. 
     */
    private RemoteRDBStoreClient rdbClient;

    /**
     * Connection to timeseries RDB
     */
    private TimeSeriesClient<Instant> tsClient;

    /**
     * Last n hours to get timeseries data for.
     */
    private int hours = 24;

    /**
     * Response to write details back to.
     */
    private HttpServletResponse response;

    /**
     * Initialise a new TimeHandler.
     * 
     * @param iri IRI of the asset.
     * @param classMatch name of class for asset.
     * @param endpoint Blazegraph endpoint for the KG.
     */
    public TimeHandler(String iri, String classMatch, ConfigEndpoint endpoint) {
        this(iri, classMatch, Arrays.asList(endpoint));
    }

    /**
     * Initialise a new TimeHandler with multiple endpoints.
     * 
     * @param iri IRI of the asset.
     * @param classMatch name of class for asset
     * @param endpoints Blazegraph endpoints for the KG.
     */
    public TimeHandler(String iri, String classMatch, List<ConfigEndpoint> endpoints) {
        String fixedIRI = iri;
        if(!fixedIRI.startsWith("<")) fixedIRI = "<" + fixedIRI;
        if(!fixedIRI.endsWith(">")) fixedIRI = fixedIRI + ">";

        this.iri = fixedIRI;
        this.classMatch = classMatch;
        this.endpoints.addAll(endpoints);
    }

    /**
     * Sets the remote store client used to connect to the KG.
     * 
     * @param rsClient KG connection client.
     * @param tsClient Timeseries client.
     */
    public void setClients(RemoteStoreClient rsClient, RemoteRDBStoreClient rdbClient, TimeSeriesClient<Instant> tsClient) {
        this.rsClient = rsClient;
        this.rdbClient = rdbClient;
        this.tsClient = tsClient;
    }

    /**
     * Queries the KG to determine measurement details using the provided timeQuery, then passes
     * these measurement IRIs to the TimeSeriesClient to get real timeseries data.
     * 
     * @param response HttpServletResponse object.
     * 
     * @return JSONArray of query result.
     * 
     * @throws Exception if anything goes wrong.
     */
    public JSONArray getData(HttpServletResponse response) throws Exception {
        this.response = response;

        if(this.endpoints.isEmpty()) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not determine any Blazegraph endpoints.\"}");
            return null;
        }

        // Lookup time limit attached to class
        this.hours = FeatureInfoAgent.CONFIG.getTimeLimit(classMatch);

        // Lookup queries attached to classes
        String queryTemplate = FeatureInfoAgent.CONFIG.getTimeQuery(this.classMatch);
        if(queryTemplate == null) {
            LOGGER.info("Could not find any timeseries queries for this class, skipping this stage.");
            return null;
        }

        // Inject parameters into query
        String query = queryTemplate.replaceAll(Pattern.quote("[IRI]"), this.iri);

        // Inject ontop endpoint
        if (query.contains("[ONTOP]")) {
            String ontopEndpoint = getOntopURL();
            if(ontopEndpoint == null) {
                response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
                response.getWriter().write("{\"description\":\"Could not determine the Ontop endpoint.\"}");
                return null;
            }
            query = query.replaceAll(Pattern.quote("[ONTOP]"), "<" + ontopEndpoint + ">");
        }

        try {
            // Run matching query
            JSONArray jsonResult = this.rsClient.executeFederatedQuery(getEndpointURLs(), query);
            if(jsonResult == null || jsonResult.length() == 0) {
                LOGGER.warn("No results regarding measurements, maybe this IRI has none?");
                return null;
            }

            // Filter the results
            jsonResult = this.filterJSON(jsonResult);

            // Transform into expected JSON objects
            return this.populateTimeseries(jsonResult);
        } catch(Exception exception) {
            LOGGER.warn("Query to get measurement details has caused an exception!");

            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"intCould not determine a valid Blazegraph endpo.\"}");
            return null;
        }
    }

    /**
     * Filter the raw JSON array returned from the KG, trimming any URLS
     *  
     * @param array raw KG results.
     */
    @SuppressWarnings("java:S3776")
    private JSONArray filterJSON(JSONArray array) {
        if(array == null || array.length() == 0) return array;

        for(int i = 0; i < array.length(); i++) {
            Map<String, String> replacements = new LinkedHashMap<>();
            JSONObject object = array.getJSONObject(i);

            Iterator<String> keyIter = object.keys();
            while(keyIter.hasNext()) {
                String key = keyIter.next();
                String value = object.get(key).toString();
                boolean toRemove = false;

                // Don't touch the measurement property
                if(key.equalsIgnoreCase(MEASUREMENT) || key.equalsIgnoreCase(FORECAST)) continue;

                // Replace underscores with spaces
                if(key.contains("_") || value.contains("_")) {
                    key = key.replaceAll(Pattern.quote("_"), " ");
                    value = value.replaceAll(Pattern.quote("_"), " ");
                    replacements.put(key, value);
                    toRemove = true;
                }
                
                if(toRemove) keyIter.remove();
            }

            // Add replacements back in
            replacements.keySet().forEach(key -> object.put(key, replacements.get(key)));
        }
        return array;
    }

    /**
     * Given the JSON array with details on measurements, this method
     * queries the Postgres endpoint to get actual timeseries data then
     * returns the final format JSONArray for return to the caller.
     * 
     * @param measurements measurement definitions from KG
     * 
     * @return timeseries details
     */
    @SuppressWarnings("java:S3776")
    private JSONArray populateTimeseries(JSONArray measurements) {
        Map<String, TimeSeries<Instant>> tsObjectList = new LinkedHashMap<>();

        try (Connection rdbConnection = rdbClient.getConnection()) {
            // Build timeseries objects for all measurements
            for(int i = 0; i < measurements.length(); i++) {
                JSONObject entry = measurements.getJSONObject(i);

                String measureIRI = entry.getString(MEASUREMENT);
                if(measureIRI == null) measureIRI = entry.getString(FORECAST);

                // Get timeseries object
                TimeSeries<Instant> tsObject = this.buildTimeseriesObject(measureIRI, rdbConnection);

                // Skip empty objects
                if(tsObject == null || !tsObject.getTimes().isEmpty()) tsObjectList.put(measureIRI, tsObject);
            }
        } catch(Exception exception) {
            LOGGER.error("Exception when using Connection in try block!", exception);
            return null;
        }
       
        // Skip if all empty
        if(tsObjectList.isEmpty()) return null;

        // Extract measurement names and units
        Map<String, String> names = new LinkedHashMap<>();
        Map<String, String> units = new LinkedHashMap<>();
        
        for(int i = 0; i < measurements.length(); i++) {
            JSONObject entry = measurements.getJSONObject(i);

            if(entry.has(MEASUREMENT)) {
                String measurementIRI = entry.getString(MEASUREMENT);
                names.put(measurementIRI, entry.optString("Name"));
                units.put(measurementIRI, entry.optString("Unit"));

            } else if(entry.has(FORECAST)) {
                String forecastIRI = entry.getString(FORECAST);
                names.put(forecastIRI, entry.optString("Name"));
                units.put(forecastIRI, entry.optString("Unit"));

            } else {
                LOGGER.warn("No 'Measurement' or 'Forecast' property found, skipping this entry.");
            }
        }

        try {
            List<Integer> ids = Arrays.stream(
                IntStream.iterate(0, x -> x < tsObjectList.size(), x -> x + 1).toArray()
            ).boxed().toList();
            
            // For some reason (unknown as there's insufficient documentation), the timeseries client requires
            // a list of maps for the names and units of each IRI, with one map per IRI. As each IRI is a measurement
            // and won't have more than a single name and unit, I don't see why this wasn't done as a single map
            // containing all IRIs.
            //
            // Until this is resolved in the TimeSeriesClient, this fudge just copies the maps N times (where N
            // is the number of measurement IRIs).
            List<Map<String, String>> fudgeNames = Collections.nCopies(tsObjectList.size(), names);
            List<Map<String, String>> fudgeUnits = Collections.nCopies(tsObjectList.size(), units);

            // Convert to JSON
            JSONArray timeseriesJSON = tsClient.convertToJSON(
                new ArrayList<>(tsObjectList.values()),
                ids,
                fudgeUnits,
                fudgeNames
            );

            // Add additionally reported parameters
            JSONArray properties = new JSONArray();

            for(int i = 0; i < measurements.length(); i++) {
                JSONObject oldEntry = measurements.getJSONObject(i);
                JSONObject newEntry = new JSONObject();

                oldEntry.keySet().forEach(key ->  {
                    if(!key.equals(MEASUREMENT) && !key.equals(FORECAST) && !key.equals("Name") && !key.equals("Unit")) {
                        newEntry.put(key, oldEntry.get(key));
                    }
                });

                properties.put(newEntry);
            }

            // Add the properties
            timeseriesJSON.getJSONObject(0).put("properties", properties);

            // Return JSON
            return timeseriesJSON;

        } catch (Exception exception) {
            LOGGER.error("Could not produce JSON objects from TimeSeries instances!", exception);

            try {
                response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
                response.getWriter().write("{\"description\":\"Could not produce JSON objects from TimeSeries instances!\"}");
            } catch(IOException ioException) {
                LOGGER.error("Could not write to HTTP response!", ioException);
            }
        }

        return null;
    }

    /**
     * Builds a timeseries object for the input measurement IRI.
     * 
     * @param measureIRI IRI of measurement
     * @param rdbConnection connection to RDB
     * 
     * @return timeseries object
     */
    protected TimeSeries<Instant> buildTimeseriesObject(String measureIRI, Connection rdbConnection) {

        // Remove the brackets from the IRI as the timeseries client is shit and can't handle them
        String fixedIRI = measureIRI;
        if (fixedIRI.startsWith("<") && fixedIRI.endsWith(">")) {
            fixedIRI = fixedIRI.substring(1, fixedIRI.length() - 1);
        }

        // Build then return the object
        TimeSeries<Instant> result = null;
        if(this.hours < 0) {
            // Get all data
            LOGGER.debug("Getting timeseries without limit...");
            result = this.tsClient.getTimeSeries(new ArrayList<>(Arrays.asList(fixedIRI)), rdbConnection);

        } else {
            LOGGER.debug("Getting timeseries with limit of last {} hours...", this.hours);

            // Determine bounds
            Instant lowerBound = LocalDateTime.now().minusHours(this.hours).toInstant(ZoneOffset.UTC);
            Instant upperBound = LocalDateTime.now().toInstant(ZoneOffset.UTC);

            result = this.tsClient.getTimeSeriesWithinBounds(
                    new ArrayList<>(Arrays.asList(fixedIRI)),
                    lowerBound,
                    upperBound,
                    rdbConnection
            );
        }

        LOGGER.debug("...call to TimeseriesClient completed.");
        LOGGER.debug(
            "Got a timeseries object with {} time values, and {} data values.",
            result.getTimes().size(),
            result.getValues(fixedIRI).size()
        );
        return result;
    }

    /**
     * Get a list of the URLs from the input endpoints.
     * 
     * @return list of endpoint urls.
     */
    private List<String> getEndpointURLs() {
        List<String> urls = new ArrayList<>();
        this.endpoints.forEach(endpoint -> urls.add(endpoint.url()));
        return urls;
    }

    /**
     * Returns the URL for the ONTOP endpoint.
     *
     * @return ONTOP url.
     */
    private String getOntopURL() {
        Optional<ConfigEndpoint> result = FeatureInfoAgent.CONFIG.getOntopEndpoint();
        if(result.isPresent()) {
            return result.get().url();
        }
        return null;
    }

}
// End of class.