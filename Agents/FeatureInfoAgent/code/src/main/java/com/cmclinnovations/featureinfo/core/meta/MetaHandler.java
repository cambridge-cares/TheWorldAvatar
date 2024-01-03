package com.cmclinnovations.featureinfo.core.meta;

import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;
import com.cmclinnovations.featureinfo.core.BaseHandler;

/**
 * This class handles querying a Blazegraph endpoint to run a predetermined
 * SPARQL query.
 */
public final class MetaHandler extends BaseHandler {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(MetaHandler.class);

    /**
     * Allowable names for colun one of query results.
     */
    private static final List<String> COLUMN_ONE = List.of("property", "Property", "label", "Label");

    /**
     * Allowable names for colun two of query results.
     */
    private static final List<String> COLUMN_TWO = List.of("value", "Value");

    /**
     * Allowable names for colun three of query results.
     */
    private static final List<String> COLUMN_THREE = List.of("unit", "Unit");

    /**
     * Name of matching class.
     */
    private final String classMatch;

    /**
     * Initialise a new MetaHandler.
     * 
     * @param iri        IRI of the asset.
     * @param classMatch name of class for asset.
     * @param endpoint   Blazegraph endpoint for the KG.
     */
    public MetaHandler(String iri, String classMatch, ConfigEndpoint endpoint) {
        this(iri, classMatch, Arrays.asList(endpoint));
    }

    /**
     * Initialise a new MetaHandler with multiple endpoints.
     * 
     * @param iri        IRI of the asset.
     * @param classMatch name of class for asset.
     * @param endpoints  Blazegraph endpoints for the KG.
     */
    public MetaHandler(String iri, String classMatch, List<ConfigEndpoint> endpoints) {
        super(iri, endpoints);
        this.classMatch = classMatch;
    }

    /**
     * Queries the KG to determine the classes representing the current IRI, then
     * finds
     * the first linked SPARQL query before executing it and returning the result.
     * 
     * @param response HttpServletResponse object.
     * 
     * @return JSONArray of query result.
     * 
     * @throws Exception if anything goes wrong.
     */
    public JSONArray getData(HttpServletResponse response) throws Exception {
        if (!this.hasEndpoints()) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not determine any Blazegraph endpoints.\"}");
            throw new IllegalStateException("Could not determine any Blazegraph endpoints.");
        }

        // Lookup queries attached to classes
        String queryTemplate = FeatureInfoAgent.CONFIG.getMetaQuery(this.classMatch);
        if (queryTemplate == null) {
            LOGGER.debug("No metadata query registered for class, will skip this phase.");
            return null;
        }
        LOGGER.info("Found and read the matching SPARQL query file.");

        try {
            // Run matching query
            return runQuery(queryTemplate, "class determination");
        } catch (IllegalStateException ex) {
            response.setStatus(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode());
            response.getWriter().write("{\"description\":\"Could not determine the Ontop endpoint.\"}");
            throw ex;
        }
    }

    /**
     * Formats the raw JSON result into something the visualisation can use.
     * 
     * @param rawResult
     * @return
     */
    @Override
    protected JSONArray formatJSON(JSONArray rawResult) {
        JSONObject jsonObject = new JSONObject();

        for (int i = 0; i < rawResult.length(); i++) {
            JSONObject entry = rawResult.getJSONObject(i);

            String key = findFirstKey(entry, COLUMN_ONE);
            String value = findFirstKey(entry, COLUMN_TWO);

            String unit = findFirstKey(entry, COLUMN_THREE);
            if (unit != null && !unit.isBlank()) {
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
    private String findFirstKey(JSONObject entry, List<String> colNames) {
        return colNames.stream().filter(entry::has).map(entry::getString).findFirst().orElse(null);
    }

}
// End of class.