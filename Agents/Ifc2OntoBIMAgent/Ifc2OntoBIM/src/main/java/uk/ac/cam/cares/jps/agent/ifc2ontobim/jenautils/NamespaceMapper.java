package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.arq.querybuilder.AbstractQueryBuilder;
import org.apache.jena.rdf.model.Model;

import java.util.HashMap;
import java.util.Map;

/**
 * Provides methods to generate the required namespaces and maps them to their prefixes.
 *
 *  @author qhouyee
 */
public class NamespaceMapper {
    /**
     * Retrieve namespace mappings from the existing model and add additional namespaces in OntoBIM.
     *
     * @param existingModel An RDF model containing existing triples and namespace.
     * @return A map containing all the namespace mappings used.
     */
    public static Map<String, String> retrieveNamespace(Model existingModel) {
        Map<String, String> nsMapping = genNamespaceMapping();
        Map<String, String> owlMapping = retrieveNamespaceFromExistingModel(existingModel);
        nsMapping.putAll(owlMapping);
        return nsMapping;
    }

    /**
     * Add the minimum template prefixes for sub-query builders.The superclass of AbstractQueryBuilder allows the
     * namespaces to be generalisable for any sparql query types.
     *
     * @param builder Abstract Query Builder object to add the prefix statements.
     */
    public static void addSubqueryBuilderNamespaces(AbstractQueryBuilder builder) {
        Map<String, String> nsMapping = genNamespaceMapping();
        builder.addPrefixes(nsMapping);
    }

    /**
     * Generates a map of namespaces and their prefixes for additional and duplicate namespaces for templates and sub-queries.
     *
     * @return A map containing the required namespace mappings.
     */
    private static Map<String, String> genNamespaceMapping() {
        Map<String, String> nsMapping = new HashMap<>();
        // Additional namespaces present in OntoBIM
        nsMapping.put("rdfs", "http://www.w3.org/2000/01/rdf-schema#");
        nsMapping.put("om", "http://www.ontology-of-units-of-measure.org/resource/om-2/");
        nsMapping.put("bim", "http://www.theworldavatar.com/kg/ontobim/");
        nsMapping.put("bot", "https://w3id.org/bot#");

        // Duplicate IfcOwl namespaces for sub-queries - require bim as well
        nsMapping.put("ifc", "http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#");
        nsMapping.put("list", "https://w3id.org/list#");
        nsMapping.put("express", "https://w3id.org/express#");
        nsMapping.put("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        return nsMapping;
    }

    /**
     * Retrieve the prefixes generated in a filled model.
     *
     * @param existingModel An RDF model containing existing triples and namespace.
     * @return A map containing the namespace mappings of the current model.
     */
    private static Map<String, String> retrieveNamespaceFromExistingModel(Model existingModel) {
        return existingModel.getNsPrefixMap();
    }
}
