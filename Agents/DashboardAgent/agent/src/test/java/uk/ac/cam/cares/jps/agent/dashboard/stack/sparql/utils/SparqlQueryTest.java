package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import static org.junit.jupiter.api.Assertions.*;

public class SparqlQueryTest {

    @Test
    void testGenSimpleFacilityQuery() {
        // Generate expected query
        String expectedQuery = genExpectedPrefixesString() +
                "SELECT DISTINCT ?facilityname" +
                " WHERE {" +
                genExpectedFacilityString() +
                "} LIMIT 1";
        // Execute and test result
        assertEquals(expectedQuery, SparqlQuery.genSimpleFacilityQuery());
    }

    @Test
    void testGenFacilityRoomMeasureQuery() {
        // Generate expected query
        String expectedQuery = genExpectedPrefixesString() +
                "SELECT DISTINCT ?orgname ?facilityname ?roomname ?measure ?measurename ?unit ?timeseries ?minthreshold ?maxthreshold " +
                "WHERE {" +
                genExpectedFacilityString() +
                "?facility ontobim:hasRoom ?room." +
                "?room rdf:type ontobim:Room;" +
                "ontobim:hasIfcRepresentation/rdfs:label ?roomname." +
                "{?room ontodevice:hasTemperature/om:hasValue ?measure.?facility ontodevice:hasMinThreshold/ontodevice:hasQuantity ?minquantity;ontodevice:hasMaxThreshold/ontodevice:hasQuantity ?maxquantity.?minquantity rdf:type om:Temperature;om:hasValue/om:hasNumericalValue ?minthreshold.?maxquantity rdf:type om:Temperature;om:hasValue/om:hasNumericalValue ?maxthreshold.}" +
                "UNION {?room ontodevice:hasRelativeHumidity/om:hasValue ?measure.?facility ontodevice:hasMinThreshold/ontodevice:hasQuantity ?minquantity;ontodevice:hasMaxThreshold/ontodevice:hasQuantity ?maxquantity.?minquantity rdf:type om:RelativeHumidity;om:hasValue/om:hasNumericalValue ?minthreshold.?maxquantity rdf:type om:RelativeHumidity;om:hasValue/om:hasNumericalValue ?maxthreshold.}" +
                genExpectedCommonMeasureString() +
                "}";
        // Execute and test result
        assertEquals(expectedQuery, SparqlQuery.genFacilityRoomMeasureQuery());
    }

    @Test
    void testGenFacilitySystemMeasureQuery() {
        // Generate expected query
        String expectedQuery = genExpectedPrefixesString() +
                "SELECT DISTINCT ?orgname ?facilityname ?systemname ?measure ?measurename ?unit ?timeseries " +
                "WHERE {" +
                genExpectedFacilityString() +
                "?facility ontotechsystem:containsSystem ?system." +
                "{?system rdfs:label ?systemname;ontoubemmp:consumesEnergy/om:hasValue ?measure.}" +
                " UNION {?system ontotechsystem:composedOf ?subsystem.?subsystem rdfs:label ?systemname;ontoubemmp:consumesEnergy/om:hasValue ?measure.}" +
                genExpectedCommonMeasureString() +
                "}";
        // Execute and test result
        assertEquals(expectedQuery, SparqlQuery.genFacilitySystemMeasureQuery());
    }

    @Test
    void testGenFacilityAssetMeasureQuery() {
        // Generate expected query
        String endpoint = "http://www.test.org/blazegraph/namespace/kb/sparql";
        String expectedQuery = genExpectedPrefixesString() +
                "SELECT DISTINCT ?orgname ?facilityname ?elementname ?elementtype ?measure ?measurename ?unit ?timeseries " +
                "WHERE {" +
                genExpectedFacilityString() +
                "?facility ontobim:hasRoom ?room." +
                "?room rdf:type ontobim:Room;bot:containsElement ?element." +
                "?element rdfs:label ?elementlabel;rdf:type ?elementtype." +
                " SERVICE <" + endpoint + ">{" +
                "{" +
                "{?element ontodevice:sendsSignalTo*/saref:consistsOf*/ontodevice:sendsSignalTo*/saref:consistsOf* ?sensor.}" +
                "UNION { ?subelement rdfs:label ?subelementname; ^saref:consistsOf ?element; ^ontodevice:isAttachedTo ?sensor.}" +
                "{?element ontodevice:measures/om:hasValue ?measure.}" +
                "UNION {?element ontodevice:observes ?measure.}" +
                "UNION {?sensor ontodevice:measures/om:hasValue ?measure.}" +
                "UNION {?sensor ontodevice:observes ?measure.}" +
                "} UNION {?element ontodevice:hasOperatingRange/ssn:hasOperatingProperty/ontodevice:hasQuantity/om:hasValue ?measure.}" +
                genExpectedCommonMeasureString() +
                "}" +
                "BIND(IF(BOUND(?subelementname), CONCAT(?elementlabel, \" \", ?subelementname), ?elementlabel) AS ?elementname)" +
                "}";
        // Execute and test result
        assertEquals(expectedQuery, SparqlQuery.genFacilityAssetMeasureQuery(endpoint));
    }

    public static String genExpectedPrefixesString() {
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

    private static String genExpectedFacilityString() {
        return "?building rdf:type bot:Building;ontobim:hasFacility ?facility." +
                "?facility <https://www.theworldavatar.com/kg/ontoassetmanagement/isManagedBy>/<https://www.omg.org/spec/Commons/Designators/hasName>/rdfs:label ?orgname;" +
                "rdfs:label ?facilityname.";
    }

    private static String genExpectedCommonMeasureString() {
        return " ?measure rdfs:label ?measurename;" +
                "ontotimeseries:hasTimeSeries ?timeseries." +
                "OPTIONAL{ ?measure om:hasUnit/om:symbol ?unit}";
    }
}