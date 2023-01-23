package uk.ac.cam.cares.jps.agent.sparql;

/**
 * Stores the String constants required for prefixes, namespaces, classes, properties, and literals.
 *
 * @author qhouyee
 */
class OntologyConstant {
    // Prefix
    protected static final String BASE_PREFIX = "twa";
    protected static final String RDF_PREFIX = "rdf";
    protected static final String RDFS_PREFIX = "rdfs";
    protected static final String XSD_PREFIX = "xsd";
    protected static final String OM_PREFIX = "om";
    protected static final String SKOS_PREFIX = "skos";
    protected static final String QUDT_PREFIX = "qudt";
    protected static final String UBEMMP_PREFIX = "ontoubemmp";
    protected static final String ONTOHEATNETWORK_PREFIX = "heatnetwork";
    protected static final String ONTOCAPE_PREFIX = "ontocape";
    protected static final String PS_PREFIX = "ps";
    // URI
    protected static final String RDF_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    protected static final String RDFS_URI = "http://www.w3.org/2000/01/rdf-schema#";
    protected static final String XSD_URI = "http://www.w3.org/2001/XMLSchema#";
    protected static final String OM_URI = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    protected static final String SKOS_URI = "http://www.w3.org/2004/02/skos/core#";
    protected static final String QUDT_URI = "http://qudt.org/schema/qudt/";
    protected static final String UBEMMP_URI = "https://www.theworldavatar.com/kg/ontoubemmp/";
    protected static final String ONTOHEATNETWORK_URI = "https://www.theworldavatar.com/kg/ontoheatnetwork/";
    protected static final String ONTOCAPE_URI = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#";
    protected static final String PS_URI = "https://www.theworldavatar.com/kg/ps/";
    // Classes
    protected static final String XSD_STRING = XSD_PREFIX + ":string";
    protected static final String OM_KWH = OM_PREFIX + ":kilowattHour";
    protected static final String OM_EUR = OM_PREFIX + ":euro";
    protected static final String UBEMMP_ELECCONSUMPTION = UBEMMP_PREFIX + ":ElectricityConsumption";
    protected static final String HEATNETWORK_COST_INTERVAL = ONTOHEATNETWORK_PREFIX + ":CostInTimeInterval";
    protected static final String PS_PUMPINGSTATION = PS_PREFIX + ":Pumpingstation";
    // Properties
    protected static final String RDFTYPE = RDF_PREFIX + ":type";
    protected static final String RDFS_LABEL = RDFS_PREFIX + ":label";
    protected static final String OM_HASVALUE = OM_PREFIX + ":hasValue";
    protected static final String OM_HASUNIT = OM_PREFIX + ":hasUnit";
    protected static final String SKOS_NOTATION = SKOS_PREFIX + ":notation";
    protected static final String UBEMMP_CONSUMES_UTILITIES = UBEMMP_PREFIX + ":consumesUtilities";
    protected static final String ONTOCAPE_HAS_UTILITY_COST = ONTOCAPE_PREFIX + ":hasUtilityCost";
    // Literals
    protected static final String KWH_LITERAL = "'kW.h'^^" + QUDT_PREFIX + ":UCUMcs";
    protected static final String COST_LITERAL = "'Stromkosten'^^" + XSD_STRING;
    protected static final String CONSUMPTION_LITERAL = "'Stromverbrauch'^^" + XSD_STRING;
}
