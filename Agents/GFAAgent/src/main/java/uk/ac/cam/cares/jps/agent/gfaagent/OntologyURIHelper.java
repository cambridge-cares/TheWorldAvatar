
package uk.ac.cam.cares.jps.agent.gfaagent;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class OntologyURIHelper {

    public static final String om = "om";
    public static final String rdf = "rdf";
    public static final String rdfs = "rdfs";
    public static final String ontobuiltenv = "ontobuiltenv";
    public static final String ic = "ic";
    public static final String twa = "twa";

    public static Map<String, String> ontologyUriMap = new HashMap<>();

    /**
     * Constructs an OntologyURIHelper object with the given ontology URIs
     * @param propertiesFileName name of the properties file
     */
    
    static {
   

            ontologyUriMap.put(om, "http://www.ontology-of-units-of-measure.org/resource/om-2/");
            ontologyUriMap.put(rdf, "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
            ontologyUriMap.put(rdfs, "http://www.w3.org/2000/01/rdf-schema#");    
            ontologyUriMap.put(ontobuiltenv, "https://www.theworldavatar.com/kg/ontobuiltenv/");
            ontologyUriMap.put(ic, "http://ontology.eil.utoronto.ca/icontact.owl#");
            ontologyUriMap.put(twa, "https://www.theworldavatar.com/kg/");


    }

    /**
     * Returns the ontology URI
     * @param ontologyName name of the ontology
     * @return ontology URI
     */
    public static String getOntologyUri(String ontologyName) {
        return ontologyUriMap.get(ontologyName);
    }
}
