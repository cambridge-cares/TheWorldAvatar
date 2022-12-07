package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcSlabQueryTest {
    private static ConstructBuilder builder;

    @BeforeEach
    void initBuilder() {
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
    }

    @Test
    void testAddSlabQueryComponentsForFloor() {
        IfcSlabQuery.addSlabQueryComponents(builder, "bim:Floor");
        String query = builder.buildString();
        String expected = this.genExpectedResultsForFloor();
        assertTrue(query.contains(expected));
    }

    @Test
    void testAddSlabQueryComponentsForRoof() {
        IfcSlabQuery.addSlabQueryComponents(builder, "bim:Roof");
        String query = builder.buildString();
        List<String> expected = this.genExpectedResultsForRoof();
        expected.forEach(line -> assertTrue(query.contains(line)));
    }

    @Test
    void testAddSlabQueryComponentsFail() {
        ConstructBuilder failBuilder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> IfcSlabQuery.addSlabQueryComponents(failBuilder, "bim:Roof"));
        assertTrue(thrownError.getMessage().contains("Predicate"));
        assertTrue(thrownError.getMessage().contains("must be a Path, URI , variable, or a wildcard."));
    }

    private String genExpectedResultsForFloor() {
        StringBuilder expected = new StringBuilder();
        expected.append("WHERE\n")
                .append("  { ?reltypedefine\n")
                .append("              rdf:type              ifc:IfcRelDefinesByType ;\n")
                .append("              ifc:relatedObjects_IfcRelDefines  ?element ;\n")
                .append("              ifc:relatingType_IfcRelDefinesByType  ?elementtype .\n")
                .append("    ?elementtype  ifc:predefinedType_IfcSlabType  ?slabEnum\n")
                .append("    VALUES ?slabEnum { ifc:FLOOR ifc:BASESLAB }\n")
                .append("  }");
        return expected.toString();
    }

    private List<String> genExpectedResultsForRoof() {
        List<String> expected = new ArrayList<>();
        // Construct statements
        expected.add("?roofzone bot:containsElement ?element");
        // Where statements
        expected.add("ifc:predefinedType_IfcSlabType  ifc:ROOF");
        expected.add("?roofrelaggregate\n" +
                "              rdf:type              ifc:IfcRelAggregates ;\n" +
                "              ifc:relatingObject_IfcRelDecomposes  ?ifcroof ;\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  ?element");
        expected.add("?relspatialcontainer\n" +
                "              rdf:type              ifc:IfcRelContainedInSpatialStructure ;\n" +
                "              ifc:relatedElements_IfcRelContainedInSpatialStructure  ?ifcroof ;\n" +
                "              ifc:relatingStructure_IfcRelContainedInSpatialStructure  ?roofzone");
        return expected;
    }
}