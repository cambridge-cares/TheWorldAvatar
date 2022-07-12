package com.cmclinnovations.featureinfo;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
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
import org.apache.jena.sparql.function.library.langeq;
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
     * @param iri Feature IRI.
     * @param endpoint KG endpoint for feature.
     */
    public Manager(String iri, String endpoint) {
       this(iri, endpoint, null);
    }

      /**
     * Initialise a new InfoGrabber instance.
     * 
     * @param iri Feature IRI.
     * @param endpoint KG endpoint for feature.
     * @param context servlet context for deployment
     */
    public Manager(String iri, String endpoint, ServletContext context) {
        this.iri = iri;
        this.endpoint = endpoint;
        this.rsClient = new RemoteStoreClient(endpoint);
        this.context = context;
    }

    /**
     * 
     */
    public void readProperties() throws Exception {
        String filePath = System.getenv("PROPERTIES_FILE");
        if(filePath == null) filePath = "feature-info.properties";

        try (InputStream inStream = new FileInputStream(filePath)) {
            this.properties = new Properties();
            this.properties.load(inStream);


            if(this.properties.getProperty("timeseries_url") == null) {
                throw new IllegalStateException("Cannot find required 'timeseries_url' key in properties!");
            }
            if(this.properties.getProperty("timeseries_user") == null) {
                throw new IllegalStateException("Cannot find required 'timeseries_user' key in properties!");
            }
            if(this.properties.getProperty("timeseries_pass") == null) {
                throw new IllegalStateException("Cannot find required 'timeseries_pass' key in properties!");
            }

            // Initialise timeseries client
            this.tsClient = new TimeSeriesClient<Instant>(
                this.rsClient,
                Instant.class,
                this.properties.get("timeseries_url").toString(),
                this.properties.get("timeseries_user").toString(),
                this.properties.get("timeseries_pass").toString()
            );
        } 
    }

    /**
     * Gather all available information.
     * 
     * @return JSONObject results in JSON form
     */
    public JSONObject grabAll() throws Exception {
        // Query KG to get metadata
        Map<String, List<String>> metadata = getMetadata();
        
        // Convert to JSON
        JSONObject metaJSON = new JSONObject();
        for(String key : metadata.keySet()) {
            if(Lookups.HIDDEN_META.contains(key.toLowerCase())) continue;

            List<String> values = metadata.get(key);
            Set<String> uniques = new LinkedHashSet<>(values);

            if(uniques.size() == 1) {
                String value = values.get(0);
                if(value != null && !value.isEmpty()) {
                    metaJSON.put(key, values.get(0));
                }
            } else {
                JSONArray array = new JSONArray();
                array.putAll(values);
                metaJSON.put(key, array);
            }
        }

        System.out.println(metaJSON.toString(2));

        // Get timeseries JSON
        JSONArray timeJSON = getTimeSeries(metadata);

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

        for(int i = 0; i < rawResult.length(); i++) {
            JSONObject entry = (JSONObject) rawResult.get(i);

            entry.keySet().forEach(key -> {
                String fixedKey = key.replaceAll(Pattern.quote("_"), " ");
                if(!metadata.containsKey(fixedKey)) metadata.put(fixedKey, new ArrayList<>());
                metadata.get(fixedKey).add(entry.optString(key));
            });
        }
        return metadata;
    }

    /**
     * 
     * @param className
     * @return
     * @throws IOException
     */
    protected String getQueryFile(String className) throws IOException {
        if(context != null) {
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
            if(classes == null || classes.length == 0) {
                throw new Exception("Classes were null or not present!");
            }
        } catch(Exception excep) {
            LOGGER.error(excep);
            throw excep;
        }

        // Find mathing query class
        for(String className : classes) {
            if(Lookups.CLASSES.containsKey(className)) {

                // Instantiate from class name
                try {
                    Class<? extends AbstractQuery> clazz = Lookups.CLASSES.get(className); 
                    Constructor<? extends AbstractQuery> constructor = clazz.getConstructor(String.class, String.class);
                    this.queryHandler = constructor.newInstance(this.iri, this.endpoint);
                    System.out.println("Using query handler: " + clazz.getSimpleName());
                } catch(Exception excep) {
                    throw new Exception("Cannot construct query handler for this feature type!");
                }
            }   
        }

        // No specific query class, try looking for a pre-written sparql file
        if(queryHandler == null) {
            for(String className : classes) {
                try {
                    String query = getQueryFile(className);
                    if(!query.isBlank()) {
                        // Sanitise IRI
                        String fixedIRI = this.iri;
                        if(!fixedIRI.startsWith("<")) fixedIRI = "<" + fixedIRI;
                        if(!fixedIRI.endsWith(">")) fixedIRI += ">";

                        query = query.replaceAll(Pattern.quote("[IRI]"), fixedIRI);
                        this.queryHandler = new WrittenQuery(fixedIRI, this.endpoint, query);
                        System.out.println("Using query handler: WrittenQuery");
                    }
                } catch(Exception excep) {
                    // Ignore, file may be missing
                    System.out.println(excep);
                }
            }
        }

        // If still no handler, bug out
        if(queryHandler == null) {
            throw new Exception("No preset query handlers for this type of feature!");
        }
        return this.queryHandler;
    }

    /**
     * 
     * @param iris
     * @return
     */
    protected JSONArray getTimeSeries( Map<String, List<String>> metadata) {
        List<String> measures = metadata.get("Measurement");

        if(measures != null && !measures.isEmpty()) {

            // Query for the timeseries data
            List<TimeSeries<Instant>> timeseries = new ArrayList<>();
            measures.forEach(iri -> {
                try {
                    TimeSeries<Instant> data = this.getTimeseries(iri);
                    if(!data.getTimes().isEmpty()) {
                        timeseries.add(data);
                    }
                } catch(Exception excep) {
                    // May have no data, skip
                }
            });

            // Combine all timeseries for this location into a single object
            TimeSeries<Instant> combined = Utils.getCombinedTimeSeries(tsClient, timeseries);

            // Convert timeseries to JSON
            Map<String, String> units = new HashMap<>();
            Map<String, String> headers = new HashMap<>();

            for(int i = 0; i < measures.size(); i++) {
                String unitStr = "";
                if(metadata.get("Measurement Unit") != null && metadata.get("Measurement Unit").size() > i) {
                    unitStr = metadata.get("Measurement Unit").get(i);
                }

                String headerStr = "";
                if(metadata.get("Measurement Parameter") != null && metadata.get("Measurement Parameter").size() > i) {

                    if(metadata.get("Measurement Quantity") != null && metadata.get("Measurement Quantity").size() > i) {
                        headerStr = metadata.get("Measurement Parameter").get(i) + " (" + metadata.get("Measurement Quantity").get(i) + ")";
                    }
                }

                units.put(measures.get(i), unitStr);
                headers.put(measures.get(i), headerStr);
            }

            try {
                JSONArray timeseriesJSON = tsClient.convertToJSON(
                    new ArrayList<>(Arrays.asList(combined)), 
                    Collections.nCopies(timeseries.size(), 1),
                    new ArrayList<>(Arrays.asList(units)), 
                    new ArrayList<>(Arrays.asList(headers))
                );
                return timeseriesJSON;
            } catch(Exception excep) {
                excep.printStackTrace(System.out);
            }
        }

        return null;
    }

    /**
     * 
     * @param kgClient
     * @param measurementIRIs
     * @return
     */
    protected TimeSeries<Instant> getTimeseries(String iri) {
        // Determine bounds (last 24 hours)
        Instant lowerBound = LocalDateTime.now().minusDays(7).toInstant(ZoneOffset.UTC);
        Instant upperBound = LocalDateTime.now().toInstant(ZoneOffset.UTC);

        // Fix the IRIs because the timeseries client is shit
        String fixedIRI = iri;
        if(iri.startsWith("<") && iri.endsWith(">")) {
            fixedIRI = iri.substring(1, iri.length() - 1);
        } 

        TimeSeries<Instant> result = this.tsClient.getTimeSeriesWithinBounds(
            new ArrayList<>(Arrays.asList(fixedIRI)),
            lowerBound,
            upperBound
        );
        
        return result;
    }

    /**
     * Run a query to determine the feature's class within the KG.
     */
    protected String[] getFeatureClasses() throws IOException {
        // Get query to determine class of feature
        String query = null;

        if(context != null) {
            try (InputStream inStream = this.context.getResourceAsStream("/WEB-INF/queries/get-class.sparql")) {
                query = IOUtils.toString(inStream, StandardCharsets.UTF_8);
            }
        } else {
            query = Files.readString(Paths.get("WEB-INF/queries/get-class.sparql"));
        }

        // Bug out or Inject IRI
        if(query == null) return null;

        String fixedIRI = iri;
        if(!fixedIRI.startsWith("<")) fixedIRI = "<" + fixedIRI;
        if(!fixedIRI.endsWith(">")) fixedIRI += ">";
        query = query.replaceAll(Pattern.quote("[IRI]"), fixedIRI);

        // Run query
        this.rsClient.setQuery(query);
        String rawResult = this.rsClient.execute();

        // Parse result as JSON
        JSONArray jsonResult = new JSONArray(rawResult);
        String[] classes = new String[jsonResult.length()];

        for(int i = 0; i < jsonResult.length(); i++) {
            JSONObject entry = jsonResult.optJSONObject(i);
            String clazz = entry.optString("class");

            // Get the last part of the class IRI
            if(clazz.contains("#")) {
                int lastHash = clazz.lastIndexOf("#");
                clazz = clazz.substring(lastHash + 1, clazz.length());
            } else {
                int lastSlash = clazz.lastIndexOf("/");
                clazz = clazz.substring(lastSlash + 1, clazz.length());
            }

            // Store class name that should match query file
            classes[i] = clazz;
        }
        
        return classes;
    }

}