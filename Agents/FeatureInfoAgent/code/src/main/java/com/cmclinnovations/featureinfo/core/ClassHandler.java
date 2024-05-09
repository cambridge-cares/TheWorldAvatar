package com.cmclinnovations.featureinfo.core;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import javax.ws.rs.InternalServerErrorException;

import org.apache.jena.util.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.objects.Request;
import com.cmclinnovations.featureinfo.utils.Utils;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class handles querying Blazegraph endpoints to determine which
 * classes the current IRI belongs to.
 */
public class ClassHandler {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ClassHandler.class);

    /**
     * Store of class mappings and endpoints
     */
    private final ConfigStore configStore;
    
    /**
     * Cached connection to KG.
     */
    private final RemoteStoreClient kgClient;

    /**
     * Cached template query to determine classes.
     */
    private String queryTemplate;

    /**
     * Initialise a new ClassHandler instance.
     * 
     * @param configStore Store of class mappings and endpoints.
     * @param kgClient Cached connection to KG.
     */
    public ClassHandler(ConfigStore configStore, RemoteStoreClient kgClient) {
        this.configStore = configStore;
        this.kgClient = kgClient;
    }

    /**
     * Runs a class determination query to get the entire class tree of the input 
     * A-Box IRI, then returns any configuration entries that use a class IRI returned
     * by that query.
     * 
     * @param iri A-Box IRI of feature.
     * @param enforcedEndpoint optional enforced Blazegraph URL.
     * 
     * @return List of configuration entries with a matching class (may be empty).
     * 
     * @throws IllegalStateException if class determination returns no matches.
     * @throws InternalServerErrorException if class determination query cannot be executed.
     */
    public List<ConfigEntry> determineClassMatches(Request request) throws IllegalStateException, InternalServerErrorException {
        // Get the list of class IRIs from the KG
        List<String> classIRIs = null;
        try {
            classIRIs = runClassQuery(request);
            if(classIRIs == null || classIRIs.isEmpty()) {
                throw new IllegalStateException("Class determination query has failed to return any results, cannot continue!");
            }
        } catch(Exception exception) {
            LOGGER.error("Running class determination query has thrown an exception!", exception);
            throw new InternalServerErrorException("Class determination query has thrown an exception, cannot continue!", exception);
        }

        // Find matches, in order returned from KG
        List<ConfigEntry> matches = new ArrayList<>();
        classIRIs.forEach(classIRI -> {
            ConfigEntry match = configStore.getConfigWithClass(classIRI);
            if(match != null) matches.add(match);
        });

        if(matches.isEmpty()) {
            throw new IllegalStateException("Class determination query had results but there were no matching IRIs in the configuration, cannot continue!");
        }
        return matches;
    }

    /**
     * Run the class determination query and return the list of class IRIs resulting from it.
     * 
     * @param iri A-Box IRI of feature.
     * @param enforcedEndpoint optional enforced Blazegraph URL.
     * 
     * @return List of class IRIs.
     * 
     * @throws Exception if KG query fails due to connection issues.
     */
    private List<String> runClassQuery(Request request) throws Exception {
        // Read the class determination SPARQL query
        loadQuery();

        // Get final query string (post injection)
        String queryString = Utils.queryInject(
                this.queryTemplate,
                request.getIri(),
                configStore.getStackEndpoints(StackEndpointType.ONTOP),
                Utils.getBlazegraphEndpoints(configStore, request.getEndpoint()));

        // Run query
        List<String> endpoints = Utils.getBlazegraphURLs(configStore, request.getEndpoint());
        JSONArray jsonResult = null;

        if(endpoints.size() == 1) {
            LOGGER.debug("Running non-federated class determination query.");
            kgClient.setQueryEndpoint(endpoints.get(0));
            jsonResult = kgClient.executeQuery(queryString);

        } else {
            LOGGER.debug("Running federated class determination query.");
            jsonResult = kgClient.executeFederatedQuery(endpoints, queryString);
        }

        // Parse and return class IRIs
        return parseJSON(jsonResult);
    }

    /**
     * Read and cache the class determination query.
     */
    private void loadQuery() {
        if(this.queryTemplate == null) {

            if(FeatureInfoAgent.CONTEXT != null) {
                // Running as a servlet
                try (InputStream inStream =  FeatureInfoAgent.CONTEXT.getResourceAsStream("WEB-INF/class-query.sparql")) {
                    this.queryTemplate = FileUtils.readWholeFileAsUTF8(inStream);
                } catch(Exception exception) {
                    LOGGER.error("Could not read the class determination query from its file!", exception);
                }
            } else {
                // Running as application/as tests
                try {
                    Path queryFile = Paths.get("WEB-INF/class-query.sparql");
                    this.queryTemplate = Files.readString(queryFile);
                } catch(IOException ioException) {
                    LOGGER.error("Could not read the class determination query from its file!", ioException);
                }
            }
        }
    }

    /**
     * Parse the JSON Array from the KG into a list of distinct strings.
     * 
     * @param rawResult raw JSON results from KG.
     * 
     * @return list of unique class IRIs.
     */
    private List<String> parseJSON(JSONArray rawResult) {
        List<String> classIRIs = new ArrayList<>();

        for(int i = 0; i < rawResult.length(); i++) {
            JSONObject entry = rawResult.getJSONObject(i);
            
            if(entry.has("class")) {
                String classIRI = entry.getString("class");
                classIRI = classIRI.replaceAll(Pattern.quote("<"), "");
                classIRI = classIRI.replaceAll(Pattern.quote(">"), "");

                if(classIRI.toLowerCase().startsWith("http")) {
                    classIRIs.add(classIRI);
                }
            }
        }
        return classIRIs;
    }

}
// End of class.