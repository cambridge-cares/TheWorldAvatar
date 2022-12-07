package com.cmclinnovations.featureinfo.kg;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.FeatureInfoAgent;
import com.cmclinnovations.featureinfo.config.ConfigEndpoint;

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
     * IRI of the asset.
     */
    private final String iri;

    /**
     * Endpoint(s) for the KG.
     */
    private final List<ConfigEndpoint> endpoints = new ArrayList<>();

     /**
     * Connection to KG.
     */
    private RemoteStoreClient rsClient;

    /**
     * Initialise a new ClassHandler.
     * 
     * @param iri IRI of the asset.
     * @param endpoint Endpoint for the KG.
     */
    public ClassHandler(String iri, ConfigEndpoint endpoint) {
        this(iri, Arrays.asList(endpoint));
    }

    /**
     * Initialise a new ClassHandler with multiple endpoints.
     * 
     * @param iri IRI of the asset.
     * @param endpoints Endpoints for the KG.
     */
    public ClassHandler(String iri, List<ConfigEndpoint> endpoints) {
        String fixedIRI = iri;
        if(!fixedIRI.startsWith("<")) fixedIRI = "<" + fixedIRI;
        if(!fixedIRI.endsWith(">")) fixedIRI = fixedIRI + ">";

        this.iri = fixedIRI;
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
     * Determines which classes the IRI belongs too, then returns the first
     * match found within the queries specified in the configuration file.
     */
    public String getClassMatch() throws Exception {
        List<String> classNames = this.getClasses();
        if(classNames == null) {
            LOGGER.error("Discovered class names array is null.");
            return null;
        }
        if(classNames.isEmpty()) {
            LOGGER.error("Discovered class names array is empty.");
            return "";
        }

        Iterator<String> iter = classNames.iterator();
        while(iter.hasNext()) {
            try {
                String className = iter.next();
                String query = FeatureInfoAgent.CONFIG.getMetaQuery(className);
                if(query != null) return className;
            } catch(IOException ioException) {
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
        String[] classes = this.runClassQuery(CLASS_QUERY);
        if(classes == null || classes.length == 0) {
            // None returned, try the Ontop query
            LOGGER.info("No classes returned from simple query, using advanced ONTOP one...");
            classes = this.runClassQuery(CLASS_QUERY_ONTOP);
        }

        if(classes != null) {
            return Arrays.asList(classes);
        }
        return null;
    }

    /**
     * Query the KG to determine the classes that the object represented by
     * the IRI inherits from.
     * 
     * @param queryTemplate template SPARQL query
     * 
     * @return array of fully qualified class names
     */
    private String[] runClassQuery(String queryTemplate) throws Exception {
        // Inject IRI into the class query
        String query = queryTemplate.replaceAll(Pattern.quote("[IRI]"), iri);

        // Inject ontop endpoint
        if (query.contains("[ONTOP]")) {
            String ontopEndpoint = this.getOntopURL();
            if(ontopEndpoint == null) {
                LOGGER.error("Could not determine Ontop endpoint!");
                return null;
            }
            query = query.replaceAll(Pattern.quote("[ONTOP]"), "<" + ontopEndpoint + ">");
        }

        LOGGER.debug("Running class determination query...");
        LOGGER.debug(query);

        // Run the federated query
        JSONArray jsonResult = rsClient.executeFederatedQuery(this.getEndpointURLs(), query);

        LOGGER.debug("...result of query was:");
        LOGGER.debug((jsonResult != null) ? jsonResult.toString(2) : "null");

        if(jsonResult != null) {
            String[] classes = new String[jsonResult.length()];
    
            for (int i = 0; i < jsonResult.length(); i++) {
                JSONObject entry = jsonResult.optJSONObject(i);
                String clazz = entry.optString("class");
                classes[i] = clazz;
            }
            return classes;
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