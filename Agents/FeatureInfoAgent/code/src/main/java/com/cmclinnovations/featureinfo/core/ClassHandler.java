package com.cmclinnovations.featureinfo.core;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;

/**
 * This class handles querying Blazegraph endpoints to determine which
 * classes the current IRI belongs to.
 */
public final class ClassHandler extends BaseHandler {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ClassHandler.class);

    /**
     * SPARQL query to get list of classes for an IRI.
     */
    public static final String CLASS_QUERY = """
                prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                SELECT distinct ?class WHERE {
                    [IRI] rdf:type ?class
                }
            """;

    /**
     * SPARQL query to get list of classes for an IRI with Ontop.
     */
    public static final String CLASS_QUERY_ONTOP = """
                prefix owl: <http://www.w3.org/2002/07/owl#>
                prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

                select distinct ?class {
                    SERVICE [ONTOP] { [IRI] a ?x . }
                    ?x rdfs:subClassOf* ?class .
                    ?class rdf:type owl:Class .
                }
            """;

    /**
     * Initialise a new ClassHandler.
     * 
     * @param iri      IRI of the asset.
     * @param endpoint Endpoint for the KG.
     */
    public ClassHandler(String iri, ConfigEndpoint endpoint) {
        this(iri, Arrays.asList(endpoint));
    }

    /**
     * Initialise a new ClassHandler with multiple endpoints.
     * 
     * @param iri       IRI of the asset.
     * @param endpoints Endpoints for the KG.
     */
    public ClassHandler(String iri, List<ConfigEndpoint> endpoints) {
        super(iri, endpoints);
    }

    /**
     * Determines which classes the IRI belongs too, then returns the first
     * match found within the queries specified in the configuration file.
     */
    public String getClassMatch() throws Exception {
        List<String> classNames = this.getClasses();
        if (classNames == null) {
            LOGGER.error("Discovered class names array is null.");
            return null;
        }
        if (classNames.isEmpty()) {
            LOGGER.error("Discovered class names array is empty.");
            return "";
        }

        Iterator<String> iter = classNames.iterator();
        while (iter.hasNext()) {
            try {
                String className = iter.next();
                String metaQuery = FeatureInfoAgent.CONFIG.getMetaQuery(className);
                String timeQuery = FeatureInfoAgent.CONFIG.getTimeQuery(className);
                if (metaQuery != null || timeQuery != null)
                    return className;
            } catch (IOException ioException) {
                // Ignore and continue
            }
        }
        return null;
    }

    /**
     * Determine the classes the object represented by the
     * current IRI inherits from.
     * 
     * @return list of full qualified classes (or null).
     */
    protected List<String> getClasses() throws Exception {
        List<String> classes = this.runClassQuery(CLASS_QUERY);
        if (classes == null || classes.isEmpty()) {
            // None returned, try the Ontop query
            LOGGER.info("No classes returned from simple query, using advanced ONTOP one...");
            classes = this.runClassQuery(CLASS_QUERY_ONTOP);
        }

        return classes;
    }

    /**
     * Query the KG to determine the classes that the object represented by
     * the IRI inherits from.
     * 
     * @param queryTemplate template SPARQL query
     * 
     * @return array of fully qualified class names
     */
    private List<String> runClassQuery(String queryTemplate) throws Exception {

        // Run the class determination query
        JSONArray jsonResult = runQuery(queryTemplate, "class determination");

        if (jsonResult != null) {
            List<String> classes = new ArrayList<>(jsonResult.length());

            for (int i = 0; i < jsonResult.length(); i++) {
                JSONObject entry = jsonResult.optJSONObject(i);
                String clazz = entry.optString("class");
                classes.add(clazz);
            }
            return classes;
        }
        return null;
    }

}
// End of class.