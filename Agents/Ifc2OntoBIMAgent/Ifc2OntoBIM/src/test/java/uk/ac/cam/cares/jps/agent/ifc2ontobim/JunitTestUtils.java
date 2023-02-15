package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.jena.arq.querybuilder.ConstructBuilder;

import java.util.HashMap;
import java.util.Map;

public class JunitTestUtils {
    public static final String bimUri = "http://www.theworldavatar.com/kg/ontobim/";
    public static final String botUri = "https://w3id.org/bot#";
    public static final String expressUri = "https://w3id.org/express#";
    public static final String ifc2x3Uri = "http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#";
    public static final String listUri = "https://w3id.org/list#";
    public static final String rdfUri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String rdfsUri = "http://www.w3.org/2000/01/rdf-schema#";

    public static void addPrefix(ConstructBuilder builder) {
        Map<String, String> nsMapping = new HashMap<>();
        nsMapping.put("rdfs", rdfsUri);
        nsMapping.put("rdf", rdfUri);
        nsMapping.put("bim", bimUri);
        nsMapping.put("bot", botUri);
        nsMapping.put("ifc", ifc2x3Uri);
        nsMapping.put("list", listUri);
        nsMapping.put("express", expressUri);
        builder.addPrefixes(nsMapping);
    }
}
