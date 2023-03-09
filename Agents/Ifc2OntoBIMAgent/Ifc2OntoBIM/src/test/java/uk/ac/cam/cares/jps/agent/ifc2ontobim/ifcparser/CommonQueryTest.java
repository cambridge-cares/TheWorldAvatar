package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.base.Sys;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class CommonQueryTest {
    private static final String TEST_ZONE_VAR = "?zone";
    private static final String TEST_ZONE_VAR_REGEX = "\\" + TEST_ZONE_VAR;

    @Test
    void testAddBaseQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addBaseQueryComponents(builder, TEST_ZONE_VAR);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedBaseQueryStatements(), builder.buildString());
    }

    @Test
    void testAddElementHostZoneQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addElementHostZoneQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedElementContainerZoneStatements(), builder.buildString());
    }

    @Test
    void testAddElementModelRepresentationQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addElementModelRepresentationQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedElementModelQueryStatements(), builder.buildString());
        assertTrue(builder.buildString().contains(genExpectedElementModelUnionQuery()));
    }

    private List<String> genExpectedBaseQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  " + TEST_ZONE_VAR_REGEX + " \\?uid \\?name \\?placement");
        expected.add(TEST_ZONE_VAR_REGEX + " ifc:globalId_IfcRoot/express:hasString \\?uid .");
        expected.add(TEST_ZONE_VAR_REGEX + " ifc:name_IfcRoot/express:hasString \\?name .");
        expected.add(TEST_ZONE_VAR_REGEX + "     ifc:objectPlacement_IfcProduct  \\?placement .");
        expected.add("\\?placement  rdf:type            ifc:IfcLocalPlacement}");
        return expected;
    }

    private List<String> genExpectedElementContainerZoneStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?subzone");
        expected.add("\\?relaggregates\n" +
                "              rdf:type              ifc:IfcRelContainedInSpatialStructure ;\n" +
                "              ifc:relatingStructure_IfcRelContainedInSpatialStructure  \\?subzone ;\n" +
                "              ifc:relatedElements_IfcRelContainedInSpatialStructure  \\?element");
        return expected;
    }

    private List<String> genExpectedElementModelQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?instshaperep \\?subcontext \\?geometry \\?shapereptype \\?geomaxisplacement \\?cartesiantransformer");
        expected.add("\\?element  ifc:representation_IfcProduct  \\?productDefinitionShape");
        expected.add("\\?productDefinitionShape\n" +
                "              rdf:type              ifc:IfcProductDefinitionShape");
        return expected;
    }

    private String genExpectedElementModelUnionQuery() {
        return "{ ?productDefinitionShape ifc:representations_IfcProductRepresentation/list:hasContents ?instshaperep .\n" +
                "        ?instshaperep\n" +
                "                  rdf:type  ifc:IfcShapeRepresentation .\n" +
                "        ?instshaperep ifc:representationType_IfcRepresentation/express:hasString ?shapereptype .\n" +
                "        ?instshaperep\n" +
                "                  ifc:contextOfItems_IfcRepresentation  ?subcontext .\n" +
                "        ?subcontext  rdf:type           ifc:IfcGeometricRepresentationSubContext .\n" +
                "        ?instshaperep\n" +
                "                  ifc:items_IfcRepresentation  ?geometry .\n" +
                "        ?geometry  rdf:type             ?geomtype\n" +
                "        FILTER ( ! regex(str(?geomtype), \"IfcMappedItem\") )\n" +
                "      }\n" +
                "    UNION\n" +
                "      { ?productDefinitionShape ifc:representations_IfcProductRepresentation/list:hasContents ?shaperep .\n" +
                "        ?shaperep  rdf:type  ifc:IfcShapeRepresentation .\n" +
                "        ?shaperep ifc:representationType_IfcRepresentation/express:hasString \"MappedRepresentation\" .\n" +
                "        ?shaperep  ifc:items_IfcRepresentation  ?mappeditem .\n" +
                "        ?mappeditem  rdf:type           ifc:IfcMappedItem ;\n" +
                "                  ifc:mappingSource_IfcMappedItem  ?representationmap ;\n" +
                "                  ifc:mappingTarget_IfcMappedItem  ?cartesiantransformer .\n" +
                "        ?cartesiantransformer\n" +
                "                  rdf:type              ifc:IfcCartesianTransformationOperator3D .\n" +
                "        ?representationmap\n" +
                "                  rdf:type              ifc:IfcRepresentationMap ;\n" +
                "                  ifc:mappingOrigin_IfcRepresentationMap  ?geomaxisplacement ;\n" +
                "                  ifc:mappedRepresentation_IfcRepresentationMap  ?instshaperep .\n" +
                "        ?instshaperep\n" +
                "                  rdf:type              ifc:IfcShapeRepresentation .\n" +
                "        ?instshaperep ifc:representationType_IfcRepresentation/express:hasString ?shapereptype .\n" +
                "        ?instshaperep\n" +
                "                  ifc:contextOfItems_IfcRepresentation  ?subcontext .\n" +
                "        ?subcontext  rdf:type           ifc:IfcGeometricRepresentationSubContext .\n" +
                "        ?instshaperep\n" +
                "                  ifc:items_IfcRepresentation  ?geometry .\n" +
                "        ?geometry  rdf:type             ?geomtype}";
    }
}