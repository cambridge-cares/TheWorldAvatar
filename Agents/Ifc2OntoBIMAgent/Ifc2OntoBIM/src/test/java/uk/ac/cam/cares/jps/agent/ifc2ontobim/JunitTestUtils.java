package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.jena.arq.querybuilder.ConstructBuilder;

import java.util.HashMap;
import java.util.Map;

public class JunitTestUtils {
    public static void addPrefix(ConstructBuilder builder) {
        Map<String, String> nsMapping = new HashMap<>();
        nsMapping.put("rdfs", "http://www.w3.org/2000/01/rdf-schema#");
        nsMapping.put("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        nsMapping.put("bim", "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#");
        nsMapping.put("bot", "https://w3id.org/bot#");
        nsMapping.put("ifc", "http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#");
        nsMapping.put("list", "https://w3id.org/list#");
        nsMapping.put("express", "https://w3id.org/express#");
        builder.addPrefixes(nsMapping);
    }
}
