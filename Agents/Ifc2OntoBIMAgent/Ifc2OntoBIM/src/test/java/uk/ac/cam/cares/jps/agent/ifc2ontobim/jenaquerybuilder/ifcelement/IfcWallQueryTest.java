package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import static org.junit.jupiter.api.Assertions.*;

class IfcWallQueryTest {
    private static ConstructBuilder builder;

    @BeforeEach
    void initBuilder() {
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
    }

    @Test
    void testAddSecondShapeRepresentationQueryComponents() {
        IfcWallQuery.addSecondShapeRepresentationQueryComponents(builder);
        String query = builder.buildString();
        String expected = this.genExpectedResults();
        assertTrue(query.contains(expected));
    }

    private String genExpectedResults() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?element bim:hasGeometricRepresentation ?secondshaperep .\n")
                .append("    ?secondshaperep rdf:type bim:ModelRepresentation3D .\n")
                .append("    ?secondshaperep bim:hasRepresentationType ?secondshapereptype .\n")
                .append("    ?secondshaperep bim:hasSubContext ?secondsubcontext .\n")
                .append("    ?secondsubcontext rdf:type bim:GeometricRepresentationSubContext .\n")
                .append("    ?secondshaperep bim:hasRepresentationItem ?secondgeometry .\n")
                .append("    ?secondgeometry rdf:type ?secondgeomtype .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { OPTIONAL\n")
                .append("      { ?productDefinitionShape (ifc:representations_IfcProductRepresentation/list:hasNext)/list:hasContents ?secondshaperep .\n")
                .append("        ?secondshaperep\n")
                .append("                  rdf:type  ifc:IfcShapeRepresentation .\n")
                .append("        ?secondshaperep ifc:representationType_IfcRepresentation/express:hasString ?secondshapereptype .\n")
                .append("        ?secondshaperep\n")
                .append("                  ifc:contextOfItems_IfcRepresentation  ?secondsubcontext .\n")
                .append("        ?secondsubcontext\n")
                .append("                  rdf:type              ifc:IfcGeometricRepresentationSubContext .\n")
                .append("        ?secondshaperep\n")
                .append("                  ifc:items_IfcRepresentation  ?secondgeometry .\n")
                .append("        ?secondgeometry\n")
                .append("                  rdf:type              ?secondgeomtype}}");
        return expected.toString();
    }
}