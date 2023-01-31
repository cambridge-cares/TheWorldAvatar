package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import static org.junit.jupiter.api.Assertions.*;

class IfcOpeningQueryTest {
    private static ConstructBuilder builder;

    @BeforeEach
    void initBuilder() {
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
    }

    @Test
    void testAddHostElementQueryComponents() {
        new IfcOpeningQuery().addHostElementQueryComponents(builder);
        String query = builder.buildString();
        String expected = this.genExpectedResultsForHostElement();
        assertTrue(query.contains(expected));
    }

    @Test
    void testAddHostElementQueryComponentsFail() {
        ConstructBuilder failBuilder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> new IfcOpeningQuery().addHostElementQueryComponents(failBuilder));
        assertTrue(thrownError.getMessage().contains("Predicate"));
        assertTrue(thrownError.getMessage().contains("must be a Path, URI , variable, or a wildcard."));
    }

    @Test
    void testAddVoidRepresentationQueryComponents() {
        new IfcOpeningQuery().addVoidRepresentationQueryComponents(builder);
        String query = builder.buildString();
        String expected = this.genExpectedResultsForVoidRep();
        assertTrue(query.contains(expected));
    }

    private String genExpectedResultsForHostElement() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?hostelement bot:hasSubElement ?element .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?relfillselement\n")
                .append("              rdf:type              ifc:IfcRelFillsElement ;\n")
                .append("              ifc:relatedBuildingElement_IfcRelFillsElement  ?element ;\n")
                .append("              ifc:relatingOpeningElement_IfcRelFillsElement  ?openingelement .\n")
                .append("    ?openingelement\n")
                .append("              rdf:type              ifc:IfcOpeningElement .\n")
                .append("    ?relvoidelement\n")
                .append("              rdf:type              ifc:IfcRelVoidsElement ;\n")
                .append("              ifc:relatedOpeningElement_IfcRelVoidsElement  ?openingelement ;\n")
                .append("              ifc:relatingBuildingElement_IfcRelVoidsElement  ?hostelement}\n");
        return expected.toString();
    }

    private String genExpectedResultsForVoidRep() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?element bim:hasVoid ?openingelement .\n")
                .append("    ?openingelement rdf:type bim:GeometricVoid .\n")
                .append("    ?openingelement bim:hasVoidType ?voidtype .\n")
                .append("    ?openingelement bim:hasLocalPosition ?voidplacement .\n")
                .append("    ?voidplacement rdf:type bim:LocalPlacement .\n")
                .append("    ?openingelement bim:hasGeometricRepresentation ?voidshaperep .\n")
                .append("    ?voidshaperep rdf:type bim:ModelRepresentation3D .\n")
                .append("    ?voidshaperep bim:hasRepresentationType ?voidreptype .\n")
                .append("    ?voidshaperep bim:hasSubContext ?voidsubcontext .\n")
                .append("    ?voidsubcontext rdf:type bim:GeometricRepresentationSubContext .\n")
                .append("    ?voidshaperep bim:hasRepresentationItem ?voidgeometry .\n")
                .append("    ?voidgeometry rdf:type ?voidgeomtype .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { OPTIONAL\n")
                .append("      { ?relvoidelement\n")
                .append("                  rdf:type              ifc:IfcRelVoidsElement ;\n")
                .append("                  ifc:relatedOpeningElement_IfcRelVoidsElement  ?openingelement ;\n")
                .append("                  ifc:relatingBuildingElement_IfcRelVoidsElement  ?element .\n")
                .append("        ?openingelement\n")
                .append("                  rdf:type              ifc:IfcOpeningElement .\n")
                .append("        ?openingelement ifc:objectType_IfcObject/express:hasString ?voidtype .\n")
                .append("        ?openingelement\n")
                .append("                  ifc:objectPlacement_IfcProduct  ?voidplacement ;\n")
                .append("                  ifc:representation_IfcProduct  ?voidproductdefinition .\n")
                .append("        ?voidproductdefinition ifc:representations_IfcProductRepresentation/list:hasContents ?voidshaperep .\n")
                .append("        ?voidshaperep\n")
                .append("                  rdf:type  ifc:IfcShapeRepresentation .\n")
                .append("        ?voidshaperep ifc:representationType_IfcRepresentation/express:hasString ?voidreptype .\n")
                .append("        ?voidshaperep\n")
                .append("                  ifc:contextOfItems_IfcRepresentation  ?voidsubcontext .\n")
                .append("        ?voidsubcontext\n")
                .append("                  rdf:type              ifc:IfcGeometricRepresentationSubContext .\n")
                .append("        ?voidshaperep\n")
                .append("                  ifc:items_IfcRepresentation  ?voidgeometry .\n")
                .append("        ?voidgeometry\n")
                .append("                  rdf:type              ?voidgeomtype}}");
        return expected.toString();
    }
}