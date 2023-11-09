package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class SparqlQueryTest {

    @Test
    void testGenSimpleFacilityQuery() {
        // Generate expected query
        StringBuilder expectedQuery = new StringBuilder();
        expectedQuery.append(genExpectedPrefixesString())
                .append("SELECT DISTINCT ?facilityname ")
                .append("WHERE {")
                .append(genExpectedFacilityString())
                .append("} LIMIT 1");
        // Execute and test result
        assertEquals(expectedQuery.toString(), SparqlQuery.genSimpleFacilityQuery());
    }

    @Test
    void testGenFacilityRoomMeasureQuery() {
        // Generate expected query
        StringBuilder expectedQuery = new StringBuilder();
        expectedQuery.append(genExpectedPrefixesString())
                .append("SELECT DISTINCT ?orgname ?facilityname ?roomname ?measure ?measurename ?unit ?timeseries ?minthreshold ?maxthreshold ")
                .append("WHERE {")
                .append(genExpectedFacilityString())
                .append("?room ontobim:hasIfcRepresentation/rdfs:label ?roomname.")
                .append("{?room ontodevice:hasTemperature/om:hasValue ?measure.?facility ontodevice:hasMinThreshold/ontodevice:hasQuantity ?minquantity;ontodevice:hasMaxThreshold/ontodevice:hasQuantity ?maxquantity.?minquantity rdf:type om:Temperature;om:hasValue/om:hasNumericalValue ?minthreshold.?maxquantity rdf:type om:Temperature;om:hasValue/om:hasNumericalValue ?maxthreshold.}")
                .append("UNION {?room ontodevice:hasRelativeHumidity/om:hasValue ?measure.?facility ontodevice:hasMinThreshold/ontodevice:hasQuantity ?minquantity;ontodevice:hasMaxThreshold/ontodevice:hasQuantity ?maxquantity.?minquantity rdf:type om:RelativeHumidity;om:hasValue/om:hasNumericalValue ?minthreshold.?maxquantity rdf:type om:RelativeHumidity;om:hasValue/om:hasNumericalValue ?maxthreshold.}")
                .append(genExpectedCommonMeasureString())
                .append("}");
        // Execute and test result
        assertEquals(expectedQuery.toString(), SparqlQuery.genFacilityRoomMeasureQuery());
    }

    @Test
    void testGenFacilitySystemMeasureQuery() {
        // Generate expected query
        StringBuilder expectedQuery = new StringBuilder();
        expectedQuery.append(genExpectedPrefixesString())
                .append("SELECT DISTINCT ?orgname ?facilityname ?systemname ?measure ?measurename ?unit ?timeseries ")
                .append("WHERE {")
                .append(genExpectedFacilityString())
                .append("?facility ontotechsystem:containsSystem ?system.")
                .append("{?system rdfs:label ?systemname;ontoubemmp:consumesEnergy/om:hasValue ?measure.}")
                .append(" UNION {?system ontotechsystem:composedOf ?subsystem.?subsystem rdfs:label ?systemname;ontoubemmp:consumesEnergy/om:hasValue ?measure.}")
                .append(genExpectedCommonMeasureString())
                .append("}");
        // Execute and test result
        assertEquals(expectedQuery.toString(), SparqlQuery.genFacilitySystemMeasureQuery());
    }

    @Test
    void testGenFacilityMeasureQuery() {
        // Generate expected query
        String endpoint = "http://www.test.org/blazegraph/namespace/kb/sparql";
        StringBuilder expectedQuery = new StringBuilder();
        expectedQuery.append(genExpectedPrefixesString())
                .append("SELECT DISTINCT ?orgname ?facilityname ?elementname ?elementtype ?measure ?measurename ?unit ?timeseries ")
                .append("WHERE {")
                .append(genExpectedFacilityString())
                .append("?room bot:containsElement ?element.")
                .append("?element rdfs:label ?elementlabel;")
                .append("   rdf:type ?elementtype.")
                .append(" SERVICE <").append(endpoint).append(">{")
                .append("{")
                .append("{?element ontodevice:sendsSignalTo*/saref:consistsOf*/ontodevice:sendsSignalTo*/saref:consistsOf* ?sensor.}")
                .append("UNION { ?subelement rdfs:label ?subelementname; ^saref:consistsOf ?element; ^ontodevice:isAttachedTo ?sensor.}")
                .append("{?element ontodevice:measures/om:hasValue ?measure.}")
                .append("UNION {?element ontodevice:observes ?measure.}")
                .append("UNION {?sensor ontodevice:measures/om:hasValue ?measure.}")
                .append("UNION {?sensor ontodevice:observes ?measure.}")
                .append("} UNION {")
                .append("?element ontodevice:hasOperatingRange/ssn:hasOperatingProperty/ontodevice:hasQuantity/om:hasValue ?measure.")
                .append("}")
                .append(genExpectedCommonMeasureString())
                .append("}")
                .append("BIND(IF(BOUND(?subelementname), CONCAT(?elementlabel, \" \", ?subelementname), ?elementlabel) AS ?elementname)")
                .append("}");
        // Execute and test result
        assertEquals(expectedQuery.toString(), SparqlQuery.genFacilityAssetMeasureQuery(endpoint));
    }

    public static StringBuilder genExpectedPrefixesString() {
        StringBuilder result = new StringBuilder();
        result.append("PREFIX bot:<https://w3id.org/bot#>")
                .append("PREFIX ontobim:<https://www.theworldavatar.com/kg/ontobim/>")
                .append("PREFIX ontodevice:<https://www.theworldavatar.com/kg/ontodevice/>")
                .append("PREFIX ontotimeseries:<https://www.theworldavatar.com/kg/ontotimeseries/>")
                .append("PREFIX ontotechsystem:<https://www.theworldavatar.com/kg/ontotechnicalsystem/>")
                .append("PREFIX ontoubemmp:<https://www.theworldavatar.com/kg/ontoubemmp/>")
                .append("PREFIX om:<http://www.ontology-of-units-of-measure.org/resource/om-2/>")
                .append("PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
                .append("PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>")
                .append("PREFIX saref:<https://saref.etsi.org/core/>")
                .append("PREFIX ssn:<http://www.w3.org/ns/ssn/systems/>");
        return result;
    }

    private static StringBuilder genExpectedFacilityString() {
        StringBuilder result = new StringBuilder();
        result.append("?building rdf:type bot:Building;")
                .append("   ontobim:hasFacility ?facility.")
                .append("?facility <https://www.theworldavatar.com/kg/ontoassetmanagement/isManagedBy>/<https://www.omg.org/spec/Commons/Designators/hasName>/rdfs:label ?orgname;")
                .append("   rdfs:label ?facilityname;")
                .append("   ontobim:hasRoom ?room.")
                .append("?room rdf:type ontobim:Room.");
        return result;
    }

    private static StringBuilder genExpectedCommonMeasureString() {
        StringBuilder result = new StringBuilder();
        result.append(" ?measure rdfs:label ?measurename;")
                .append("ontotimeseries:hasTimeSeries ?timeseries.")
                .append("OPTIONAL{ ?measure om:hasUnit/om:symbol ?unit}");
        return result;
    }
}