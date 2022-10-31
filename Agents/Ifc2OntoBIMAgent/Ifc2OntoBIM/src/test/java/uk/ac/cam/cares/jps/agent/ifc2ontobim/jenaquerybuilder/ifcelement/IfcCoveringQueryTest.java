package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import static org.junit.jupiter.api.Assertions.*;

class IfcCoveringQueryTest {
    private static ConstructBuilder builder;

    @BeforeEach
    void initBuilder() {
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
    }

    @Test
    void testAddCoveringQueryComponentsForCeiling() {
        IfcCoveringQuery.addCoveringQueryComponents(builder, "bim:Ceiling");
        String query = builder.buildString();
        String expected = this.genExpectedResultsForCeiling();
        assertTrue(query.contains(expected));
    }

    private String genExpectedResultsForCeiling() {
        StringBuilder expected = new StringBuilder();
        expected.append("?reltypedefine\n")
                .append("              rdf:type              ifc:IfcRelDefinesByType ;\n")
                .append("              ifc:relatedObjects_IfcRelDefines  ?element ;\n")
                .append("              ifc:relatingType_IfcRelDefinesByType  ?elementtype .\n")
                .append("    ?elementtype  rdf:type          ifc:IfcCoveringType ;\n")
                .append("              ifc:predefinedType_IfcCoveringType  ifc:CEILING");
        return expected.toString();
    }
}