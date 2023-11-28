package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import static org.junit.jupiter.api.Assertions.*;

class IfcProjectConstructBuilderTest {
    @Test
    void testCreateSparqlQuery() {
        ConstructBuilder builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
        String query = new IfcProjectConstructBuilder().createSparqlQuery(builder);
        String expected = this.genExpectedResults();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryFail() {
        ConstructBuilder builder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> new IfcProjectConstructBuilder().createSparqlQuery(builder));
        assertTrue(thrownError.getMessage().contains("Predicate (\"rdf:type\") must be a Path, URI , variable, or a wildcard."));
    }

    private String genExpectedResults() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?project rdfs:label ?name .\n")
                .append("    ?project bim:hasPhase ?phase .\n")
                .append("    ?project bim:hasContext ?repcontext .\n")
                .append("    ?project bim:hasRootZone ?root .\n")
                .append("    ?repcontext rdf:type bim:GeometricRepresentationContext .\n")
                .append("    ?repcontext bim:hasSpaceDimensions ?spacedimensions .\n")
                .append("    ?repcontext bim:hasPrecision ?modelprecision .\n")
                .append("    ?repcontext bim:hasTrueNorth ?northdirection .\n")
                .append("    ?repcontext bim:hasWorldCoordinateSystem ?modelplacement .\n")
                .append("    ?northdirection rdf:type bim:DirectionVector .\n")
                .append("    ?modelplacement rdf:type bim:LocalPlacement .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?project  rdf:type  ifc:IfcProject .\n")
                .append("    ?project ifc:longName_IfcProject/express:hasString ?name .\n")
                .append("    ?project ifc:phase_IfcProject/express:hasString ?phase .\n")
                .append("    ?project  ifc:representationContexts_IfcProject  ?repcontext .\n")
                .append("    ?relaggregates\n")
                .append("              rdf:type              ifc:IfcRelAggregates ;\n")
                .append("              ifc:relatingObject_IfcRelDecomposes  ?project ;\n")
                .append("              ifc:relatedObjects_IfcRelDecomposes  ?root .\n")
                .append("    ?repcontext  rdf:type           ifc:IfcGeometricRepresentationContext ;\n")
                .append("              ifc:worldCoordinateSystem_IfcGeometricRepresentationContext  ?modelplacement .\n")
                .append("    ?repcontext ifc:coordinateSpaceDimension_IfcGeometricRepresentationContext/express:hasInteger ?spacedimensions .\n")
                .append("    ?repcontext ifc:precision_IfcGeometricRepresentationContext/express:hasDouble ?modelprecision .\n")
                .append("    ?repcontext  ifc:trueNorth_IfcGeometricRepresentationContext  ?northdirection}\n");
        return expected.toString();
    }
}