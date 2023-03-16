package com.cmclinnovations.featureinfo.kg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class handles querying a Blazegraph endpoint to run a predetermined SPARQL query.
 */
public class MetaHandler {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(MetaHandler.class);

    /**
     * Allowable names for colun one of query results.
     */
    private static final String[] COLUMN_ONE = new String[]{"property", "Property", "label", "Label"};

     /**
     * Allowable names for colun two of query results.
     */
    private static final String[] COLUMN_TWO = new String[]{"value", "Value"};

     /**
     * Allowable names for colun three of query results.
     */
    private static final String[] COLUMN_THREE = new String[]{"unit", "Unit"};

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
     * Initialise a new MetaHandler.
     * 
     * @param iri IRI of the asset.
     * @param classMatch name of class for asset.
     * @param endpoint Blazegraph endpoint for the KG.
     */
    public MetaHandler(String iri, String classMatch, ConfigEndpoint endpoint) {
        this(iri, classMatch, Arrays.asList(endpoint));
    }

    /**
     * Initialise a new MetaHandler with multiple endpoints.
     * 
     * @param iri IRI of the asset.
     * @param classMatch name of class for asset.
     * @param endpoints Blazegraph endpoints for the KG.
     */
    public MetaHandler(String iri, String classMatch, List<ConfigEndpoint> endpoints) {
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
     */
    public void setClient(RemoteStoreClient rsClient) {
        this.rsClient = rsClient;
    }

    /**
     * Queries the KG to determine the classes representing the current IRI, then finds
     * the first linked SPARQL query before executing it and returning the result.
     * 
     * @param response HttpServletResponse object.
     * 
     * @return JSONArray of query result.
     * 
     * @throws Exception if anything goes wrong.
     */
    public JSONArray getData(HttpServletResponse response) throws Exception {
        if(this.endpoints.isEmpty()) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not determine any Blazegraph endpoints.\"}");
            throw new IllegalStateException("Could not determine any Blazegraph endpoints.");
        }

        // Lookup queries attached to classes
        String queryTemplate = FeatureInfoAgent.CONFIG.getMetaQuery(this.classMatch);
        if(queryTemplate == null) {
           LOGGER.debug("No metadata query registered for class, will skip this phase.");
           return null;
        }
        LOGGER.info("Found and read the matching SPARQL query file.");

        // Inject parameters into query
        String query = queryTemplate.replaceAll(Pattern.quote("[IRI]"), this.iri);

        // Inject ontop endpoint
        if (query.contains("[ONTOP]")) {
            String ontopEndpoint = getOntopURL();
            if(ontopEndpoint == null) {
                response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
                response.getWriter().write("{\"description\":\"Could not determine the Ontop endpoint.\"}");
                throw new IllegalStateException("Could not determine the Ontop endpoint.");
            }
            query = query.replaceAll(Pattern.quote("[ONTOP]"), "<" + ontopEndpoint + ">");
        }

        // Run matching query
        LOGGER.info("Running federated meta query...");
        JSONArray rawResult = this.rsClient.executeFederatedQuery(getEndpointURLs(), query);
        LOGGER.info("...receieved response from RemoteStoreClient.");

        // Format and return
        if(rawResult != null) LOGGER.debug(rawResult.toString(2));
        return formatJSON(rawResult);
    }

    /**
     * Formats the raw JSON result into something the visualisation can use.
     * 
     * @param rawResult
     * @return
     */
    private JSONArray formatJSON(JSONArray rawResult) {
        JSONObject jsonObject = new JSONObject();

        for(int i = 0; i < rawResult.length(); i++) {
            JSONObject entry = rawResult.getJSONObject(i);

            String key = findFirstKey(entry, COLUMN_ONE);
            String value =  findFirstKey(entry, COLUMN_TWO);

            String unit = findFirstKey(entry, COLUMN_THREE);
            if(unit != null && !unit.isBlank()) {
                value += " [" + unit + "]";
            }
            
            jsonObject.put(key, value);
        }
        JSONArray jsonArray = new JSONArray();
        jsonArray.put(jsonObject);

        LOGGER.info("Meta data content has been formatted for visualisation.");
        return jsonArray;
    }

    /**
     * 
     * @param entry
     * @return
     */
    private String findFirstKey(JSONObject entry, String[] colNames) {
        for(int i = 0; i < colNames.length; i++){
            if(entry.has(colNames[i])) return entry.getString(colNames[i]);
        }
        return null;
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