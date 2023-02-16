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
    public static final String RDFS_PREFIX = "rdfs";
    public static final String RDFS_NAMESPACE= "http://www.w3.org/2000/01/rdf-schema#";
    public static final String RDF_PREFIX = "rdf";
    public static final String RDF_NAMESPACE= "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String BIM_PREFIX = "bim";
    public static final String BIM_NAMESPACE = "http://www.theworldavatar.com/kg/ontobim/";
    public static final String BOT_PREFIX = "bot";
    public static final String BOT_NAMESPACE = "https://w3id.org/bot#";
    public static final String OM_PREFIX = "om";
    public static final String OM_NAMESPACE = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String SKOS_PREFIX = "skos";
    public static final String SKOS_NAMESPACE = "http://www.w3.org/2004/02/skos/core#";
    public static final String IFC_PREFIX = "ifc";
    public static final String IFC_NAMESPACE= "http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#";
    public static final String LIST_PREFIX = "list";
    public static final String LIST_NAMESPACE= "https://w3id.org/list#";
    public static final String EXPRESS_PREFIX = "express";
    public static final String EXPRESS_NAMESPACE= "https://w3id.org/express#";
    private static String baseNamespace = "http://default.org/";
    private static final String basePrefix = "inst";

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
        nsMapping.put(RDFS_PREFIX, RDFS_NAMESPACE);
        nsMapping.put(OM_PREFIX, OM_NAMESPACE);
        nsMapping.put(BIM_PREFIX, BIM_NAMESPACE);
        nsMapping.put(BOT_PREFIX, BOT_NAMESPACE);
        nsMapping.put(SKOS_PREFIX, SKOS_NAMESPACE);

        // Duplicate IfcOwl namespaces for sub-queries - require bim as well
        nsMapping.put(IFC_PREFIX, IFC_NAMESPACE);
        nsMapping.put(LIST_PREFIX, LIST_NAMESPACE);
        nsMapping.put(EXPRESS_PREFIX, EXPRESS_NAMESPACE);
        nsMapping.put(RDF_PREFIX, RDF_NAMESPACE);
        nsMapping.put(basePrefix, baseNamespace);
        return nsMapping;
    }

    /**
     * Retrieve the prefixes generated in a filled model.
     *
     * @param existingModel An RDF model containing existing triples and namespace.
     * @return A map containing the namespace mappings of the current model.
     */
    private static Map<String, String> retrieveNamespaceFromExistingModel(Model existingModel) {
        baseNamespace = existingModel.getNsPrefixMap().get(basePrefix);
        return existingModel.getNsPrefixMap();
    }
}
