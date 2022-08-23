package com.cmclinnovations.featureinfo;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Pattern;

import javax.servlet.ServletContext;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.queries.AbstractQuery;
import com.cmclinnovations.featureinfo.queries.WrittenQuery;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Controller class that handles running multiple queries, or contacting other
 * clients, to grab info for an individual feature.
 */
public class Manager {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(Manager.class);

    /**
     * Feature IRI.
     */
    private final String iri;

    /**
     * KG endpoint.
     */
    private final String endpoint;

    /**
     * Connection to KG.
     */
    private final RemoteStoreClient rsClient;

    /**
     * Servlet context needed to read files
     */
    private ServletContext context;

    /**
     * Handles running specific queries.
     */
    private AbstractQuery queryHandler;

    /**
     * Properties.
     */
    private Properties properties;

    /**
     * 
     */
    private TimeSeriesClient<Instant> tsClient;

    /**
     * Initialise a new InfoGrabber instance.
     * 
     * @param iri      Feature IRI.
     * @param endpoint KG endpoint for feature.
     */
    public Manager(String iri, String endpoint) {
        this(iri, endpoint, null);
    }

    /**
     * Initialise a new InfoGrabber instance.
     * 
     * @param iri      Feature IRI.
     * @param endpoint KG endpoint for feature.
     * @param context  servlet context for deployment
     */
    public Manager(String iri, String endpoint, ServletContext context) {
        this.iri = iri;

        if (endpoint == null || endpoint.isEmpty()) {
            // Get the blazegraph endpoint from the stack config
            this.endpoint = Config.bg_url;
            System.out.println("No endpoint set, using the stack's Blazegraph one.");
            System.out.println("Blazegraph endpoint is: " + endpoint);
        } else {
            this.endpoint = endpoint;
        }

        this.rsClient = new RemoteStoreClient(this.endpoint);
        this.context = context;
    }

    /**
     * 
     */
    public void readProperties() throws Exception {
        // Initialise timeseries client
        this.tsClient = new TimeSeriesClient<Instant>(
                this.rsClient,
                Instant.class,
                Config.pg_url,
                Config.pg_user,
                Config.pg_password);
    }

    /**
     * Gather all available information.
     * 
     * @return JSONObject results in JSON form
     */
    public JSONObject grabAll() throws Exception {
        // Query KG to get metadata
        Map<String, List<String>> metadata = getMetadata();
        LOGGER.info("Got the metadata.");

        // Convert to JSON
        JSONObject metaJSON = toJSON(metadata);

        // Get timeseries JSON
        JSONArray timeJSON = null;
        try {
            List<String> iris = new ArrayList<>();
            if (metadata.get("Measurement") != null)
                iris.addAll(metadata.get("Measurement"));
            if (metadata.get("Forecast") != null)
                iris.addAll(metadata.get("Forecast"));

            System.out.println("Measurement IRIs are...");
            iris.forEach(iri -> System.out.println(iri));

            // Get the timeseries data objects (keyed by IRI)
            Map<String, TimeSeries<Instant>> tsObjects = getTimeseriesObjects(iris);
            System.out.println("Got timeseries instances: " + tsObjects.size());

            timeJSON = getTimeseriesJSON(metadata, tsObjects);
            LOGGER.info("Got the observed and forecast timeseries (if present).");

        } catch (Exception excep) {
            LOGGER.error(excep);
        }

        // Combine into single object and return
        JSONObject result = new JSONObject();
        result.put("meta", metaJSON);
        result.put("time", timeJSON);
        return result;
    }

    /**
     * Query feature class, determine query template, then run query at KG endpoint.
     * 
     * @return query results.
     * @throws Exception
     */
    protected Map<String, List<String>> getMetadata() throws Exception {
        this.setupQueryHandler();
        JSONArray rawResult = this.queryHandler.getMetadata();

        // Group the metadata
        HashMap<String, List<String>> metadata = new LinkedHashMap<>();

        for (int i = 0; i < rawResult.length(); i++) {
            JSONObject entry = (JSONObject) rawResult.get(i);

            entry.keySet().forEach(key -> {
                String fixedKey = key.replaceAll(Pattern.quote("_"), " ");
                if (!metadata.containsKey(fixedKey))
                    metadata.put(fixedKey, new ArrayList<>());

                String value = entry.optString(key);
                if (value == null) {
                    System.out.println("Null value for key: " + key);

                } else {

                    // Misc fudging
                    if (fixedKey.equals("Location") && value.contains("#")) {
                        String[] parts = value.split(Pattern.quote("#"));
                        value = parts[1] + ", " + parts[0];
                    }
                    if (fixedKey.equals("Elevation")) {
                        value = value + "m";
                    }
                    if (fixedKey.endsWith(" Unit")) {
                        value = Lookups.UNITS.get(value);
                    }
                    if (fixedKey.endsWith(" Quantities")) {
                        String[] parts = value.split("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])");
                        value = String.join(" ", parts);
                    }

                    metadata.get(fixedKey).add(value);
                }
            });
        }
        return metadata;
    }

    /**
     * 
     * @param metaMap
     */
    protected JSONObject toJSON(Map<String, List<String>> metaMap) {
        // Convert to JSON
        JSONObject metaJSON = new JSONObject();

        for (String key : metaMap.keySet()) {
            if (Lookups.HIDDEN_META.contains(key.toLowerCase()))
                continue;

            // If there exists a capitalised version of this key, skip this one
            String upper = key.substring(0, 1).toUpperCase() + key.substring(1);
            if (!upper.equals(key) && metaMap.containsKey(upper)) {
                System.out.println("Map contains " + key + " and " + upper + ", skipping former");
                continue;
            }

            List<String> values = metaMap.get(key);
            Set<String> uniques = new LinkedHashSet<>(values);

            if (uniques.size() == 1) {
                String value = values.get(0);
                if (value != null && !value.isEmpty()) {
                    metaJSON.put(key, values.get(0));
                }
            } else {
                JSONArray array = new JSONArray();
                array.putAll(values);
                metaJSON.put(key, array);
            }
        }

        LOGGER.info("Converted metadata to JSON.");
        return metaJSON;
    }

    /**
     * 
     * @param className
     * @return
     * @throws IOException
     */
    protected String getQueryFile(String className) throws IOException {
        if (context != null) {
            String fileName = "/WEB-INF/queries/" + className + ".sparql";
            System.out.println(fileName);
            try (InputStream inStream = context.getResourceAsStream(fileName)) {
                return IOUtils.toString(inStream, StandardCharsets.UTF_8);
            }
        } else {
            String fileName = "WEB-INF/queries/" + className + ".sparql";
            System.out.println(fileName);
            return Files.readString(Paths.get(fileName));
        }
    }

    /**
     * 
     * @throws Exception
     */
    protected AbstractQuery setupQueryHandler() throws Exception {
        // Get the feature's class(es)
        String[] classes;
        try {
            classes = getFeatureClasses();
            if (classes == null || classes.length == 0) {
                throw new Exception("Classes were null or not present!");
            }
        } catch (Exception excep) {
            LOGGER.error(excep);
            throw excep;
        }

        // No specific query class, try looking for a pre-written sparql file
        if (queryHandler == null) {
            boolean done = false;

            for (String className : classes) {
                if (done)
                    continue;

                String query = null;
                if (Lookups.FILES.containsKey(className)) {
                    query = getQueryFile(Lookups.FILES.get(className));
                } else {
                    query = "";
                }

                try {
                    if (!query.isBlank()) {
                        // Sanitise IRI
                        String fixedIRI = this.iri;
                        if (!fixedIRI.startsWith("<"))
                            fixedIRI = "<" + fixedIRI;
                        if (!fixedIRI.endsWith(">"))
                            fixedIRI += ">";

                        // Inject feature IRI
                        query = query.replaceAll(Pattern.quote("[IRI]"), fixedIRI);

                        // Inject ontop endpoint
                        if (query.contains("[ONTOP]")) {
                            String ontopEndpoint = "<" + Config.ot_url + ">";
                            query = query.replaceAll(Pattern.quote("[ONTOP]"), ontopEndpoint);
                            System.out.println("Ontop endpoint is: " + ontopEndpoint);
                        }

                        // Build handler to run query
                        this.queryHandler = new WrittenQuery(fixedIRI, this.endpoint, this.rsClient, query);
                        done = true;
                    }
                } catch (Exception excep) {
                    // Ignore, file may be missing
                    System.out.println(excep);
                }
            }
        }

        // If still no handler, bug out
        if (queryHandler == null) {
            throw new Exception("No preset query handlers for this type of feature!");
        }
        return this.queryHandler;
    }

    /**
     * Given a list of measurement or forecast IRIs, this method uses the
     * TimeseriesClient to
     * query, populate, and return TimeSeries<Instant> instances.
     * 
     * @param iris measurement and/or forecast IRIs.
     * 
     * @return resulting TimeSeries<Instant> instances.
     */
    protected Map<String, TimeSeries<Instant>> getTimeseriesObjects(List<String> iris) {
        Map<String, TimeSeries<Instant>> timeseries = new HashMap<>();

        iris.forEach(iri -> {
            try {
                TimeSeries<Instant> data = this.getTimeseriesObject(iri);
                if (!data.getTimes().isEmpty()) {
                    // Skip instantiated timeseries with no data
                    timeseries.put(iri, data);
                }
            } catch (Exception excep) {
                // May not be instantiated, skip
                LOGGER.warn("Could not get timeseries instance for IRI: " + iri);
            }
        });

        return timeseries;
    }

    /**
     * 
     * @param iris
     * @return
     */
    protected JSONArray getTimeseriesJSON(Map<String, List<String>> metadata,
            Map<String, TimeSeries<Instant>> timeseries) {
        if (timeseries.isEmpty()) {
            return new JSONArray();
        }

        // Combine all timeseries for this location into a single object
        LOGGER.info("Combining timeseries instances...");
        List<TimeSeries<Instant>> tsList = new ArrayList<>(timeseries.values());
        TimeSeries<Instant> combined = Utils.getCombinedTimeSeries(tsClient, tsList);
        LOGGER.info("Combined timeseries instances into a single instance.");

        // Convert timeseries to JSON
        Map<String, String> units = new HashMap<>();
        Map<String, String> headers = new HashMap<>();

        timeseries.keySet().forEach(iri -> {
            // Very dodgy way of handling this (worry about later)

            if (iri.contains("Measure")) {
                int index = metadata.get("Measurement").indexOf(iri);
                String unit = metadata.get("Measurement Unit").get(index);
                units.put(iri, unit);

                if (unit != null && unit.contains(";")) {
                    System.out.println(unit);
                }

                String quantity = metadata.get("Measured Quantities").get(index);
                headers.put(iri, quantity);

            } else if (iri.contains("Forecast")) {
                int index = metadata.get("Forecast").indexOf(iri);
                String unit = metadata.get("Forecast Symbol").get(index);
                units.put(iri, unit);

                if (unit != null && unit.contains(";")) {
                    System.out.println(unit);
                }

                String quantity = metadata.get("Forecast Quantities").get(index);
                quantity += " (Forecast)";
                headers.put(iri, quantity);
            }
        });

        try {
            JSONArray timeseriesJSON = tsClient.convertToJSON(
                    new ArrayList<>(Arrays.asList(combined)),
                    Collections.nCopies(timeseries.size(), 1),
                    new ArrayList<>(Arrays.asList(units)),
                    new ArrayList<>(Arrays.asList(headers)));
            return timeseriesJSON;
        } catch (Exception excep) {
            excep.printStackTrace(System.out);
        }

        return null;
    }

    /**
     * 
     * @param kgClient
     * @param measurementIRIs
     * @return
     */
    protected TimeSeries<Instant> getTimeseriesObject(String iri) {
        // Determine bounds (last 24 hours)
        Instant lowerBound = LocalDateTime.now().minusDays(28).toInstant(ZoneOffset.UTC);
        Instant upperBound = LocalDateTime.now().toInstant(ZoneOffset.UTC);

        // Fix the IRIs because the timeseries client is shit
        String fixedIRI = iri;
        if (iri.startsWith("<") && iri.endsWith(">")) {
            fixedIRI = iri.substring(1, iri.length() - 1);
        }

        TimeSeries<Instant> result = this.tsClient.getTimeSeriesWithinBounds(
                new ArrayList<>(Arrays.asList(fixedIRI)),
                lowerBound,
                upperBound);

        return result;
    }

    /**
     * Run a query to determine the feature's class within the KG.
     */
    protected String[] getFeatureClasses() throws IOException {
        String[] classes = getFeatureClasses("get-class.sparql");

        if (classes.length == 0) {
            return getFeatureClasses("get-class-credo.sparql");
        }
        return classes;
    }

    /**
     * Run a query to determine the feature's class within the KG.
     */
    protected String[] getFeatureClasses(String queryFile) throws IOException {
        if (queryFile == null)
            queryFile = "get-class.sparql";

        // Get query to determine class of feature
        String query = null;

        if (context != null) {
            try (InputStream inStream = this.context.getResourceAsStream("/WEB-INF/queries/" + queryFile)) {
                query = IOUtils.toString(inStream, StandardCharsets.UTF_8);
            }
        } else {
            query = Files.readString(Paths.get("WEB-INF/queries/get-class.sparql"));
        }

        // Bug out or Inject IRI
        if (query == null)
            return null;

        String fixedIRI = iri;
        if (!fixedIRI.startsWith("<"))
            fixedIRI = "<" + fixedIRI;
        if (!fixedIRI.endsWith(">"))
            fixedIRI += ">";

        // Inject feature IRI
        query = query.replaceAll(Pattern.quote("[IRI]"), fixedIRI);

        // Inject ontop endpoint
        if (query.contains("[ONTOP]")) {
            String ontopEndpoint = "<" + Config.ot_url + ">";
            query = query.replaceAll(Pattern.quote("[ONTOP]"), ontopEndpoint);
        }

        // Run query
        this.rsClient.setQuery(query);
        String rawResult = this.rsClient.execute();

        // Parse result as JSON
        JSONArray jsonResult = new JSONArray(rawResult);
        String[] classes = new String[jsonResult.length()];

        for (int i = 0; i < jsonResult.length(); i++) {
            JSONObject entry = jsonResult.optJSONObject(i);
            String clazz = entry.optString("class");
            classes[i] = clazz;
        }

        return classes;
    }

}