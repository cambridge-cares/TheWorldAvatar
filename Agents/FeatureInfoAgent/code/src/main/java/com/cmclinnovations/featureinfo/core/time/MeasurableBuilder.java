package com.cmclinnovations.featureinfo.core.time;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import org.apache.jena.util.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.utils.Utils;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class handles both parsing a JSONObject into an internal Measurable object,
 * and querying the KG for the IRI of the time series instance that is the parent of a
 * Measurable instance (if that wasn't provided during the initial query to get the
 * Measurable's IRI).
 */
public class MeasurableBuilder {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(MeasurableBuilder.class);

    /**
     * SPARQL query template.
     */
    private static String QUERY_TEMPLATE;

    /**
     * Constructor.
     */
    private MeasurableBuilder() {
        // No
    }

    /**
     * Parses the input JSONObject into a measurement object.
     * 
     * @param classMatch config entry responsible for this result.
     * @param jsonObj raw result from KG.
     */
    public static Measurable build(ConfigEntry classMatch, JSONObject jsonObj) {
        // Parse all fields from JSON
        Object measureIRI = TimeParser.findField(TimeParser.KEYS[0], jsonObj);
        if(measureIRI == null) {
            measureIRI = TimeParser.findField("Measurement", jsonObj);
            if(measureIRI == null) {
                measureIRI = TimeParser.findField("Forecast", jsonObj);
            }
        }

        Object timeIRI = TimeParser.findField(TimeParser.KEYS[1], jsonObj);
        if(timeIRI == null) {
            timeIRI = TimeParser.findField("timeseries", jsonObj);
        }
        
        Object name = TimeParser.findField(TimeParser.KEYS[2], jsonObj);
        Object unit = TimeParser.findField(TimeParser.KEYS[3], jsonObj);

        // Bug out if required fields not present
        if(measureIRI == null || name == null) return null;

        // Create instance
        Measurable measure = new Measurable(
            measureIRI.toString(),
            name.toString(),
            (unit == null) ? null : unit.toString(),
            classMatch
        );

        // Set the parent timeseries IRI if present
        if(timeIRI != null && !timeIRI.toString().isEmpty()) {
            measure.setParentTimeSeries(timeIRI.toString());
        }
        return measure;
    }

    /**
     * Given a measurable instance, this runs a SPARQL query to determine the IRI of the
     * linked time series instance then stores it within the measurable instance.
     * 
     * @param measurable measurable instance.
     * @param configStore store of KG endpoints.
     * @param enforcedEndpoint optional enforced KG endpoint.
     * @param kgClient connection to KG.
     * 
     * @throws Exception if SPARQL execution fails.
     */
    public static void populateTimeSeriesIRIs(
        Measurable measurable, 
        ConfigStore configStore, 
        Optional<String> enforcedEndpoint,
        RemoteStoreClient kgClient) throws Exception {

        // Bug out if already set
        if(measurable.getTimeSeriesIRI() != null) return;

        // Get IRI of measurable entity
        String measurableIRI = measurable.getEntityIRI();

        // Load KG query
        loadQuery();

        // Get final query string (post injection)
        String queryString = Utils.queryInject(
            QUERY_TEMPLATE,
            measurableIRI,
            configStore.getStackEndpoints(StackEndpointType.ONTOP),
            Utils.getBlazegraphEndpoints(configStore, enforcedEndpoint)
        );

        // Run query
        List<String> endpoints = Utils.getBlazegraphURLs(configStore, enforcedEndpoint);
        JSONArray jsonResult = null;

        if(endpoints.size() == 1) {
            LOGGER.debug("Running non-federated measurement population query.");
            kgClient.setQueryEndpoint(endpoints.get(0));
            jsonResult = kgClient.executeQuery(queryString);

        } else {
            LOGGER.debug("Running federated measurement population query.");
            jsonResult = kgClient.executeFederatedQuery(endpoints, queryString);
        }

        // Set parent time series IRI using first result
        if(jsonResult != null && jsonResult.length() > 0) {
            String timeseriesIRI = jsonResult.getJSONObject(0).optString("timeseries");
            measurable.setParentTimeSeries(timeseriesIRI);
        }
    }

     /**
     * Read and cache the class determination query.
     */
    private static void loadQuery() {
        if(QUERY_TEMPLATE == null) {

            if(FeatureInfoAgent.CONTEXT != null) {
                // Running as a servlet
                try (InputStream inStream =  FeatureInfoAgent.CONTEXT.getResourceAsStream("WEB-INF/measurable-query.sparql")) {
                    QUERY_TEMPLATE = FileUtils.readWholeFileAsUTF8(inStream);
                } catch(Exception exception) {
                    LOGGER.error("Could not read the measurement population query from its file!", exception);
                }
            } else {
                // Running as application/as tests
                try {
                    Path queryFile = Paths.get("WEB-INF/measurable-query.sparql");
                    QUERY_TEMPLATE = Files.readString(queryFile);
                } catch(IOException ioException) {
                    LOGGER.error("Could not read the measurement population query from its file!", ioException);
                }
            }
        }
    }

}
// End of class.