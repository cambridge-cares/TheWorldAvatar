package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

/**
 * A class that generates the required SPARQL queries.
 *
 * @author qhouyee
 */
public class SparqlQuery {
    public static final String ORGANISATION_NAME = "orgname";
    public static final String FACILITY_NAME = "facilityname";
    public static final String ROOM_NAME = "roomname";
    public static final String SYSTEM_NAME = "systemname";
    public static final String ELEMENT_NAME = "elementname";
    public static final String ELEMENT_TYPE = "elementtype";
    public static final String MEASURE = "measure";
    public static final String MEASURE_NAME = "measurename";
    public static final String UNIT = "unit";
    public static final String TIME_SERIES = "timeseries";
    public static final String MIN_THRESHOLD = "minthreshold";
    public static final String MAX_THRESHOLD = "maxthreshold";
    private static final String SELECT_CLAUSE = "SELECT DISTINCT";
    private static final String WHERE_CLAUSE = " WHERE {";

    // Private constructor to prevent instantiation.
    private SparqlQuery() {
    }

    /**
     * Generate a simple SPARQL query for facilities. This query is meant to detect which namespace contains the
     * building hierarchy. Even one result will indicate the namespace contains the hierarchy.
     *
     * @return The query for execution.
     */
    public static String genSimpleFacilityQuery() {
        return genPrefixes() +
                SELECT_CLAUSE +
                StringHelper.formatSparqlVarName(FACILITY_NAME) +
                WHERE_CLAUSE +
                genFacilitySyntax() +
                // Limit the results to reduce performance overhead
                "} LIMIT 1";
    }

    /**
     * Generates a dynamic query for all measures associated with rooms
     * that can be retrieved from the current and external namespace holding the time series triples.
     *
     * @return The query for execution.
     */
    public static String genFacilityRoomMeasureQuery() {
        return genPrefixes() +
                SELECT_CLAUSE +
                StringHelper.formatSparqlVarName(ORGANISATION_NAME) +
                StringHelper.formatSparqlVarName(FACILITY_NAME) +
                StringHelper.formatSparqlVarName(ROOM_NAME) +
                StringHelper.formatSparqlVarName(MEASURE) +
                StringHelper.formatSparqlVarName(MEASURE_NAME) +
                StringHelper.formatSparqlVarName(UNIT) +
                StringHelper.formatSparqlVarName(TIME_SERIES) +
                StringHelper.formatSparqlVarName(MIN_THRESHOLD) +
                StringHelper.formatSparqlVarName(MAX_THRESHOLD) +
                WHERE_CLAUSE +
                // Query to get rooms within a facility
                genFacilitySyntax() +
                "?facility ontobim:hasRoom ?room." +
                "?room rdf:type ontobim:Room;" +
                "ontobim:hasIfcRepresentation/rdfs:label" + StringHelper.formatSparqlVarName(ROOM_NAME) + "." +
                // Possible measures are temperature and relative humidity
                "{?room ontodevice:hasTemperature/om:hasValue" + StringHelper.formatSparqlVarName(MEASURE) + "." +
                "?facility ontodevice:hasMinThreshold/ontodevice:hasQuantity ?minquantity;ontodevice:hasMaxThreshold/ontodevice:hasQuantity ?maxquantity." +
                "?minquantity rdf:type om:Temperature;om:hasValue/om:hasNumericalValue" + StringHelper.formatSparqlVarName(MIN_THRESHOLD) + "." +
                "?maxquantity rdf:type om:Temperature;om:hasValue/om:hasNumericalValue" + StringHelper.formatSparqlVarName(MAX_THRESHOLD) + "." +
                "}" +
                "UNION {?room ontodevice:hasRelativeHumidity/om:hasValue" + StringHelper.formatSparqlVarName(MEASURE) + "." +
                "?facility ontodevice:hasMinThreshold/ontodevice:hasQuantity ?minquantity;ontodevice:hasMaxThreshold/ontodevice:hasQuantity ?maxquantity." +
                "?minquantity rdf:type om:RelativeHumidity;om:hasValue/om:hasNumericalValue" + StringHelper.formatSparqlVarName(MIN_THRESHOLD) + "." +
                "?maxquantity rdf:type om:RelativeHumidity;om:hasValue/om:hasNumericalValue" + StringHelper.formatSparqlVarName(MAX_THRESHOLD) + "." +
                "}" +
                genCommonMeasureSyntax() +
                "}";
    }

    /**
     * Generates a query for all measures associated with systems
     * that can be retrieved from the current namespace holding the time series triples.
     *
     * @return The query for execution.
     */
    public static String genFacilitySystemMeasureQuery() {
        return genPrefixes() +
                SELECT_CLAUSE +
                StringHelper.formatSparqlVarName(ORGANISATION_NAME) +
                StringHelper.formatSparqlVarName(FACILITY_NAME) +
                StringHelper.formatSparqlVarName(SYSTEM_NAME) +
                StringHelper.formatSparqlVarName(MEASURE) +
                StringHelper.formatSparqlVarName(MEASURE_NAME) +
                StringHelper.formatSparqlVarName(UNIT) +
                StringHelper.formatSparqlVarName(TIME_SERIES) +
                WHERE_CLAUSE +
                // Query to get systems within a facility
                genFacilitySyntax() +
                "?facility ontotechsystem:containsSystem ?system." +
                // Sub query to retrieve the system's energy consumption measures
                "{" +
                "?system rdfs:label" + StringHelper.formatSparqlVarName(SYSTEM_NAME) + ";" +
                "ontoubemmp:consumesEnergy/om:hasValue" + StringHelper.formatSparqlVarName(MEASURE) + "." +
                "} UNION {" +
                // Second sub query to retrieve the subsystem's energy consumption measures
                "?system ontotechsystem:composedOf ?subsystem." +
                "?subsystem rdfs:label" + StringHelper.formatSparqlVarName(SYSTEM_NAME) + ";" +
                "ontoubemmp:consumesEnergy/om:hasValue" + StringHelper.formatSparqlVarName(MEASURE) + "." +
                "}" +
                genCommonMeasureSyntax() +
                "}";
    }

    /**
     * Generates a dynamic query for all measures associated with assets
     * that can be retrieved from the current and external namespace holding the time series triples.
     *
     * @param endpoint The non-spatial zone SPARQL endpoint.
     * @return The query for execution.
     */
    public static String genFacilityAssetMeasureQuery(String endpoint) {
        return genPrefixes() +
                SELECT_CLAUSE +
                StringHelper.formatSparqlVarName(ORGANISATION_NAME) +
                StringHelper.formatSparqlVarName(FACILITY_NAME) +
                StringHelper.formatSparqlVarName(ELEMENT_NAME) +
                StringHelper.formatSparqlVarName(ELEMENT_TYPE) +
                StringHelper.formatSparqlVarName(MEASURE) +
                StringHelper.formatSparqlVarName(MEASURE_NAME) +
                StringHelper.formatSparqlVarName(UNIT) +
                StringHelper.formatSparqlVarName(TIME_SERIES) +
                WHERE_CLAUSE +
                // Query to get assets within a facility
                genFacilitySyntax() +
                "?facility ontobim:hasRoom ?room." +
                "?room rdf:type ontobim:Room;bot:containsElement ?element." +
                "?element rdfs:label ?elementlabel;rdf:type" + StringHelper.formatSparqlVarName(ELEMENT_TYPE) + "." +
                // Query to retrieve the time series associated with devices at a separate endpoint
                " SERVICE <" + endpoint + ">{" +
                // The below line performs a recursive query to retrieve all sub devices in the possible permutations of:
                // Device sendsSignalTo subDevice; sendsSignalTo/consistsOf subDevice; consistsOf subDevice;
                // consistsOf/sendsSignalTo subDevice; consistsOf/sendsSignalTo/consistsOf subDevice
                "{" +
                // Sensors may be linked to the element in two ways
                // First way is through sendsSignalTo and consistsOf
                "{?element ontodevice:sendsSignalTo*/saref:consistsOf*/ontodevice:sendsSignalTo*/saref:consistsOf* ?sensor.}" +
                // Second way is through consistsOf and isAttachedTo
                "UNION { ?subelement rdfs:label ?subelementname; ^saref:consistsOf ?element; ^ontodevice:isAttachedTo ?sensor.}" +
                // Retrieve the measure and its name associated with either the element or their subdevices
                "{?element ontodevice:measures/om:hasValue" + StringHelper.formatSparqlVarName(MEASURE) + ".}" +
                "UNION {?element ontodevice:observes" + StringHelper.formatSparqlVarName(MEASURE) + ".}" +
                "UNION {?sensor ontodevice:measures/om:hasValue" + StringHelper.formatSparqlVarName(MEASURE) + ".}" +
                "UNION {?sensor ontodevice:observes" + StringHelper.formatSparqlVarName(MEASURE) + ".}" +
                "} UNION {" +
                // For non-sensor devices which is being measured by sensors attached to them
                "?element ontodevice:hasOperatingRange/ssn:hasOperatingProperty/ontodevice:hasQuantity/om:hasValue" + StringHelper.formatSparqlVarName(MEASURE) + "." +
                "}" +
                genCommonMeasureSyntax() +
                "}" +
                // If there is a sub element name, append it to the element label to get element name
                // Else, element label should be the element name
                "BIND(IF(BOUND(?subelementname), CONCAT(?elementlabel, \" \", ?subelementname), ?elementlabel) AS" + StringHelper.formatSparqlVarName(ELEMENT_NAME) + ")" +
                "}";
    }

    /**
     * Generate the prefixes for all queries.
     *
     * @return The prefixes as a string builder.
     */
    private static String genPrefixes() {
        return "PREFIX bot:<https://w3id.org/bot#>" +
                "PREFIX ontobim:<https://www.theworldavatar.com/kg/ontobim/>" +
                "PREFIX ontodevice:<https://www.theworldavatar.com/kg/ontodevice/>" +
                "PREFIX ontotimeseries:<https://www.theworldavatar.com/kg/ontotimeseries/>" +
                "PREFIX ontotechsystem:<https://www.theworldavatar.com/kg/ontotechnicalsystem/>" +
                "PREFIX ontoubemmp:<https://www.theworldavatar.com/kg/ontoubemmp/>" +
                "PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>" +
                "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>" +
                "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>" +
                "PREFIX saref:<https://saref.etsi.org/core/>" +
                "PREFIX ssn:<http://www.w3.org/ns/ssn/systems/>";
    }

    /**
     * Generate the common facility syntax for the corresponding queries.
     *
     * @return The syntax as a string builder.
     */
    private static String genFacilitySyntax() {
        return "?building rdf:type bot:Building;ontobim:hasFacility ?facility." +
                "?facility <https://www.theworldavatar.com/kg/ontoassetmanagement/isManagedBy>/<https://www.omg.org/spec/Commons/Designators/hasName>/rdfs:label" +
                StringHelper.formatSparqlVarName(ORGANISATION_NAME) + ";" +
                "rdfs:label" + StringHelper.formatSparqlVarName(FACILITY_NAME) + ".";
    }

    /**
     * Generate the common measure syntax for the corresponding queries.
     *
     * @return The syntax as a string builder.
     */
    private static String genCommonMeasureSyntax() {
        // Retrieves measure name
        return StringHelper.formatSparqlVarName(MEASURE) + " rdfs:label" + StringHelper.formatSparqlVarName(MEASURE_NAME) + ";" +
                // Retrieves time series IRI
                "ontotimeseries:hasTimeSeries" + StringHelper.formatSparqlVarName(TIME_SERIES) + "." +
                // Retrieves unit if it is available
                "OPTIONAL{" + StringHelper.formatSparqlVarName(MEASURE) + " om:hasUnit/om:symbol" + StringHelper.formatSparqlVarName(UNIT) + "}";
    }
}
