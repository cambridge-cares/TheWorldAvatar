package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.base.Sys;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcSpatialZonesConstructBuilderTest {
    private static ConstructBuilder builder;

    @BeforeEach
    void initBuilder() {
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
    }

    @Test
    void testCreateSparqlQueryForSite() {
        String query = new IfcSpatialZonesConstructBuilder().createSparqlQuery(builder, "ifc:IfcSite", "bim:IfcSiteRepresentation");
        // Check for statements specific to site
        List<String> expected = this.genExpectedResultsForSite();
        System.out.println(query);
        expected.forEach(line -> assertTrue(query.contains(line)));
    }

    @Test
    void testCreateSparqlQueryFail() {
        ConstructBuilder failBuilder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> new IfcSpatialZonesConstructBuilder().createSparqlQuery(failBuilder, "ifc:IfcSite", "bim:Site"));
        assertTrue(thrownError.getMessage().contains("Predicate"));
        assertTrue(thrownError.getMessage().contains("must be a Path, URI , variable, or a wildcard."));
    }

    private List<String> genExpectedResultsForSite() {
        List<String> expected = new ArrayList<>();
        // Construct statements
        expected.add("?element bim:hasRefLatitude ?latcompoundangle .");
        expected.add("?latcompoundangle rdf:type bim:CompoundPlaneAngle .");
        expected.add("?latcompoundangle bim:hasDegree ?latdegree .");
        expected.add("?latcompoundangle bim:hasMinute ?latminute .");
        expected.add("?latcompoundangle bim:hasSecond ?latsecond .");
        expected.add("?latcompoundangle bim:hasMillionthSecond ?latmilsecond .");
        expected.add("?element bim:hasRefLongitude ?longcompoundangle .");
        expected.add("?longcompoundangle rdf:type bim:CompoundPlaneAngle .");
        expected.add("?longcompoundangle bim:hasDegree ?longdegree .");
        expected.add("?longcompoundangle bim:hasMinute ?longminute .");
        expected.add("?longcompoundangle bim:hasSecond ?longsecond .");
        expected.add("?longcompoundangle bim:hasMillionthSecond ?longmilsecond .");
        // Where statements
        expected.add("?element  ifc:refLatitude_IfcSite  ?latcompoundangle .");
        expected.add("?latcompoundangle list:hasContents/express:hasInteger ?latdegree .");
        expected.add("?latcompoundangle (list:hasNext/list:hasContents)/express:hasInteger ?latminute .");
        expected.add("?latcompoundangle ((list:hasNext/list:hasNext)/list:hasContents)/express:hasInteger ?latsecond");
        expected.add("?latcompoundangle (((list:hasNext/list:hasNext)/list:hasNext)/list:hasContents)/express:hasInteger ?latmilsecond");
        expected.add("?element  ifc:refLongitude_IfcSite  ?longcompoundangle .");
        expected.add("?longcompoundangle list:hasContents/express:hasInteger ?longdegree .");
        expected.add("?longcompoundangle (list:hasNext/list:hasContents)/express:hasInteger ?longminute .");
        expected.add("?longcompoundangle ((list:hasNext/list:hasNext)/list:hasContents)/express:hasInteger ?longsecond");
        expected.add("?longcompoundangle (((list:hasNext/list:hasNext)/list:hasNext)/list:hasContents)/express:hasInteger ?longmilsecond");
        return expected;
    }
}