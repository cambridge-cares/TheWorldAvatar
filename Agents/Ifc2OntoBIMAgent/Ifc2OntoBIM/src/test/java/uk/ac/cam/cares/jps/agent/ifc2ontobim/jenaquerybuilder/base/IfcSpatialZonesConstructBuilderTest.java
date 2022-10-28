package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
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
    void testCreateSparqlQueryForSpace() {
        String query = new IfcSpatialZonesConstructBuilder().createSparqlQuery(builder, "ifc:IfcSpace", "bot:Space");
        // In the first test, check all query statements are included
        String expected = this.genExpectedResultsForSpace();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForBuildingStorey() {
        String query = new IfcSpatialZonesConstructBuilder().createSparqlQuery(builder, "ifc:IfcBuildingStorey", "bot:Storey");
        // Check for statements specific to building storey
        List<String> expected = this.genExpectedResultsForBuildingStorey();
        expected.forEach(line -> assertTrue(query.contains(line)));
    }

    @Test
    void testCreateSparqlQueryForBuilding() {
        String query = new IfcSpatialZonesConstructBuilder().createSparqlQuery(builder, "ifc:IfcBuilding", "bot:Building");
        // Check for statements specific to building
        List<String> expected = this.genExpectedResultsForBuilding();
        expected.forEach(line -> assertTrue(query.contains(line)));
    }

    @Test
    void testCreateSparqlQueryForSite() {
        String query = new IfcSpatialZonesConstructBuilder().createSparqlQuery(builder, "ifc:IfcSite", "bot:Site");
        // Check for statements specific to site
        List<String> expected = this.genExpectedResultsForSite();
        expected.forEach(line -> assertTrue(query.contains(line)));
    }

    @Test
    void testCreateSparqlQueryFail() {
        ConstructBuilder failBuilder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> new IfcSpatialZonesConstructBuilder().createSparqlQuery(failBuilder, "ifc:IfcSite", "bot:Site"));
        assertTrue(thrownError.getMessage().contains("Predicate"));
        assertTrue(thrownError.getMessage().contains("must be a Path, URI , variable, or a wildcard."));
    }

    private String genExpectedResultsForSpace() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?element rdf:type bot:Space .\n")
                .append("    ?element bim:hasIfcId ?uid .\n")
                .append("    ?element rdfs:label ?name .\n")
                .append("    ?element bim:hasLocalPosition ?localplacement .\n")
                .append("    ?localplacement rdf:type bim:LocalPlacement .\n")
                .append("    ?zone bot:hasSpace ?element .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?element  rdf:type  ifc:IfcSpace .\n")
                .append("    ?element ifc:globalId_IfcRoot/express:hasString ?uid .\n")
                .append("    ?element ifc:name_IfcRoot/express:hasString ?name .\n")
                .append("    ?element  ifc:objectPlacement_IfcProduct  ?localplacement .\n")
                .append("    ?localplacement\n")
                .append("              rdf:type              ifc:IfcLocalPlacement .\n")
                .append("    ?relaggregates\n")
                .append("              rdf:type              ifc:IfcRelAggregates ;\n")
                .append("              ifc:relatingObject_IfcRelDecomposes  ?zone ;\n")
                .append("              ifc:relatedObjects_IfcRelDecomposes  ?element}\n");
        return expected.toString();
    }

    private List<String> genExpectedResultsForBuildingStorey() {
        List<String> expected = new ArrayList<>();
        expected.add("?element rdf:type bot:Storey .");
        expected.add("?zone bot:hasStorey ?element .");
        expected.add("?element  rdf:type  ifc:IfcBuildingStorey .");

        expected.add("?element bim:hasRefElevation ?refelevation");
        expected.add("?element ifc:elevation_IfcBuildingStorey/express:hasDouble ?refelevation");
        return expected;

    }

    private List<String> genExpectedResultsForBuilding() {
        List<String> expected = new ArrayList<>();
        expected.add("?element rdf:type bot:Building .");
        expected.add("?zone bot:hasBuilding ?element ");
        expected.add("?element  rdf:type  ifc:IfcBuilding .");
        return expected;
    }

    private List<String> genExpectedResultsForSite() {
        List<String> expected = new ArrayList<>();
        // Construct statements
        expected.add("?element rdf:type bot:Site .");
        expected.add("?element bim:hasRefElevation ?elevation .");
        expected.add("?element bim:hasRefLatitude ?latcompoundangle .");
        expected.add("?latcompoundangle rdf:type bim:Latitude .");
        expected.add("?latcompoundangle bim:hasDegree ?latdegree .");
        expected.add("?latcompoundangle bim:hasMinute ?latminute .");
        expected.add("?latcompoundangle bim:hasSecond ?latsecond .");
        expected.add("?latcompoundangle bim:hasMillionthSecond ?latmilsecond .");
        expected.add("?element bim:hasRefLongitude ?longcompoundangle .");
        expected.add("?longcompoundangle rdf:type bim:Longitude .");
        expected.add("?longcompoundangle bim:hasDegree ?longdegree .");
        expected.add("?longcompoundangle bim:hasMinute ?longminute .");
        expected.add("?longcompoundangle bim:hasSecond ?longsecond .");
        expected.add("?longcompoundangle bim:hasMillionthSecond ?longmilsecond .");
        // Where statements
        expected.add("?element  rdf:type  ifc:IfcSite .");
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
        expected.add("?element ifc:refElevation_IfcSite/express:hasDouble ?elevation");
        return expected;
    }
}