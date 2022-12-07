package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcElementConstructBuilderTest {
    private static ConstructBuilder builder;

    @BeforeEach
    void initBuilder() {
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
    }

    @Test
    void testCreateSparqlQueryForColumn() {
        String query = new IfcElementConstructBuilder().createSparqlQuery(builder, "ifc:IfcColumn", "bim:Column");
        String expected = this.genExpectedResultsForColumn();
        // In the first test, check all common query statements are included
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForDoor() {
        String query = new IfcElementConstructBuilder().createSparqlQuery(builder, "ifc:IfcDoor", "bim:Door");
        // Check that some additional statements for door are called correctly
        // Not all statements are verified as the relevant test has ensured output is correct
        String expected = this.genExpectedResultsForDoor();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForFloor() {
        String query = new IfcElementConstructBuilder().createSparqlQuery(builder, "ifc:IfcSlabF", "bim:Floor");
        // Check that some additional statements for floor are called correctly
        // Not all statements are verified as the relevant test has ensured output is correct
        List<String> expected = this.genExpectedResultsForFloor();
        expected.forEach(line -> assertTrue(query.contains(line)));
    }

    @Test
    void testCreateSparqlQueryForStair() {
        String query = new IfcElementConstructBuilder().createSparqlQuery(builder, "ifc:IfcStair", "bim:Stair");
        // Check that some additional statements for stair are called correctly
        // Not all statements are verified as the relevant test has ensured output is correct
        List<String> expected = this.genExpectedResultsForStair();
        expected.forEach(line -> assertTrue(query.contains(line)));
        // This query component must not be included
        StringBuilder excluded = new StringBuilder();
        appendGeometricRepresentationQueryComponents(excluded);
        assertFalse(query.contains(excluded.toString()));
    }

    @Test
    void testCreateSparqlQueryForRoof() {
        String query = new IfcElementConstructBuilder().createSparqlQuery(builder, "ifc:IfcSlabR", "bim:Roof");
        // This query component must not be included
        StringBuilder excluded = new StringBuilder();
        appendSpatialLocationQueryComponents(excluded);
        assertFalse(query.contains(excluded.toString()));
    }

    @Test
    void testCreateSparqlQueryFail() {
        ConstructBuilder failBuilder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> new IfcElementConstructBuilder().createSparqlQuery(failBuilder, "ifc:IfcDoor", "bim:Door"));
        assertTrue(thrownError.getMessage().contains("Predicate"));
        assertTrue(thrownError.getMessage().contains("must be a Path, URI , variable, or a wildcard."));
    }

    private String genExpectedResultsForColumn() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?element rdf:type bim:Column .\n")
                .append("    ?element bim:hasIfcId ?uid .\n")
                .append("    ?element rdfs:label ?name .\n")
                .append("    ?element bim:hasLocalPosition ?localplacement .\n")
                .append("    ?localplacement rdf:type bim:LocalPlacement .\n")
                .append("    ?zone bot:containsElement ?element .\n")
                .append("    ?element bim:hasGeometricRepresentation ?instshaperep .\n")
                .append("    ?instshaperep rdf:type bim:ModelRepresentation3D .\n")
                .append("    ?instshaperep bim:hasRepresentationType ?shapereptype .\n")
                .append("    ?instshaperep bim:hasSubContext ?subcontext .\n")
                .append("    ?subcontext rdf:type bim:GeometricRepresentationSubContext .\n")
                .append("    ?instshaperep bim:hasRepresentationItem ?geometry .\n")
                .append("    ?geometry rdf:type ?geomtype .\n")
                .append("    ?instshaperep bim:hasTargetPlacement ?cartesiantransformer .\n")
                .append("    ?cartesiantransformer rdf:type bim:CartesianTransformationOperator .\n")
                .append("    ?instshaperep bim:hasSourcePlacement ?geomaxisplacement .\n")
                .append("    ?geomaxisplacement rdf:type bim:LocalPlacement .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?element  rdf:type  ifc:IfcColumn .\n")
                .append("    ?element ifc:globalId_IfcRoot/express:hasString ?uid .\n")
                .append("    ?element ifc:name_IfcRoot/express:hasString ?name .\n")
                .append("    ?element  ifc:objectPlacement_IfcProduct  ?localplacement .\n")
                .append("    ?localplacement\n")
                .append("              rdf:type              ifc:IfcLocalPlacement .\n")
                .append("    ");
        appendSpatialLocationQueryComponents(expected);
        expected.append("    ?element  ifc:representation_IfcProduct  ?productDefinitionShape .\n")
                .append("    ?productDefinitionShape\n")
                .append("              rdf:type              ifc:IfcProductDefinitionShape\n")
                .append("      ");
        appendGeometricRepresentationQueryComponents(expected);
        return expected.toString();
    }

    private void appendSpatialLocationQueryComponents(StringBuilder builder) {
        builder.append("?spatialStructureRelationship\n")
                .append("              rdf:type              ifc:IfcRelContainedInSpatialStructure ;\n")
                .append("              ifc:relatedElements_IfcRelContainedInSpatialStructure  ?element ;\n")
                .append("              ifc:relatingStructure_IfcRelContainedInSpatialStructure  ?zone .\n");
    }

    private void appendGeometricRepresentationQueryComponents(StringBuilder builder) {
        builder.append("{ ?productDefinitionShape ifc:representations_IfcProductRepresentation/list:hasContents ?instshaperep .\n")
                .append("        ?instshaperep\n")
                .append("                  rdf:type  ifc:IfcShapeRepresentation .\n")
                .append("        ?instshaperep ifc:representationType_IfcRepresentation/express:hasString ?shapereptype .\n")
                .append("        ?instshaperep\n")
                .append("                  ifc:contextOfItems_IfcRepresentation  ?subcontext .\n")
                .append("        ?subcontext  rdf:type           ifc:IfcGeometricRepresentationSubContext .\n")
                .append("        ?instshaperep\n")
                .append("                  ifc:items_IfcRepresentation  ?geometry .\n")
                .append("        ?geometry  rdf:type             ?geomtype\n")
                .append("        FILTER ( ! regex(str(?geomtype), \"IfcMappedItem\") )\n")
                .append("      }\n")
                .append("    UNION\n")
                .append("      { ?productDefinitionShape ifc:representations_IfcProductRepresentation/list:hasContents ?shaperep .\n")
                .append("        ?shaperep  rdf:type  ifc:IfcShapeRepresentation .\n")
                .append("        ?shaperep ifc:representationType_IfcRepresentation/express:hasString \"MappedRepresentation\" .\n")
                .append("        ?shaperep  ifc:items_IfcRepresentation  ?mappeditem .\n")
                .append("        ?mappeditem  rdf:type           ifc:IfcMappedItem ;\n")
                .append("                  ifc:mappingSource_IfcMappedItem  ?representationmap ;\n")
                .append("                  ifc:mappingTarget_IfcMappedItem  ?cartesiantransformer .\n")
                .append("        ?cartesiantransformer\n")
                .append("                  rdf:type              ifc:IfcCartesianTransformationOperator3D .\n")
                .append("        ?representationmap\n")
                .append("                  rdf:type              ifc:IfcRepresentationMap ;\n")
                .append("                  ifc:mappingOrigin_IfcRepresentationMap  ?geomaxisplacement ;\n")
                .append("                  ifc:mappedRepresentation_IfcRepresentationMap  ?instshaperep .\n")
                .append("        ?instshaperep\n")
                .append("                  rdf:type              ifc:IfcShapeRepresentation .\n")
                .append("        ?instshaperep ifc:representationType_IfcRepresentation/express:hasString ?shapereptype .\n")
                .append("        ?instshaperep\n")
                .append("                  ifc:contextOfItems_IfcRepresentation  ?subcontext .\n")
                .append("        ?subcontext  rdf:type           ifc:IfcGeometricRepresentationSubContext .\n")
                .append("        ?instshaperep\n")
                .append("                  ifc:items_IfcRepresentation  ?geometry .\n")
                .append("        ?geometry  rdf:type             ?geomtype");
    }

    private String genExpectedResultsForDoor() {
        StringBuilder expected = new StringBuilder();
        expected.append("?relfillselement\n")
                .append("              rdf:type              ifc:IfcRelFillsElement ;\n")
                .append("              ifc:relatedBuildingElement_IfcRelFillsElement  ?element ;\n")
                .append("              ifc:relatingOpeningElement_IfcRelFillsElement  ?openingelement .\n")
                .append("    ?openingelement\n")
                .append("              rdf:type              ifc:IfcOpeningElement .\n")
                .append("    ?relvoidelement\n")
                .append("              rdf:type              ifc:IfcRelVoidsElement ;\n")
                .append("              ifc:relatedOpeningElement_IfcRelVoidsElement  ?openingelement ;\n")
                .append("              ifc:relatingBuildingElement_IfcRelVoidsElement  ?hostelement");
        return expected.toString();
    }

    private List<String> genExpectedResultsForFloor() {
        List<String> expected = new ArrayList<>();
        expected.add("?reltypedefine\n" +
                "              rdf:type              ifc:IfcRelDefinesByType ;\n" +
                "              ifc:relatedObjects_IfcRelDefines  ?element ;\n" +
                "              ifc:relatingType_IfcRelDefinesByType  ?elementtype .");
        expected.add("?elementtype  ifc:predefinedType_IfcSlabType  ?slabEnum");
        expected.add("VALUES ?slabEnum { ifc:FLOOR ifc:BASESLAB }");
        return expected;
    }

    private List<String> genExpectedResultsForStair() {
        List<String> expected = new ArrayList<>();
        expected.add("?relaggregates\n" +
                "              rdf:type              ifc:IfcRelAggregates ;\n" +
                "              ifc:relatingObject_IfcRelDecomposes  ?element ;\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  ?stairflight .");
        expected.add("?relaggregates\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  ?landing .");
        expected.add("?landing  rdf:type              ifc:IfcSlab .");
        expected.add("?landing  ifc:objectPlacement_IfcProduct  ?landingplacement ;\n" +
                "              ifc:representation_IfcProduct  ?landingdefinition .\n" +
                "    ?landingdefinition ifc:representations_IfcProductRepresentation/list:hasContents ?landingshaperep .\n" +
                "    ?landingshaperep\n" +
                "              rdf:type  ifc:IfcShapeRepresentation .\n" +
                "    ?landingshaperep ifc:representationType_IfcRepresentation/express:hasString ?landingshapereptype .\n" +
                "    ?landingshaperep\n" +
                "              ifc:contextOfItems_IfcRepresentation  ?landingcontext .\n" +
                "    ?landingcontext\n" +
                "              rdf:type              ifc:IfcGeometricRepresentationSubContext .\n" +
                "    ?landingshaperep\n" +
                "              ifc:items_IfcRepresentation  ?landinggeom .\n" +
                "    ?landinggeom  rdf:type          ?landinggeomtype .");
        return expected;
    }
}