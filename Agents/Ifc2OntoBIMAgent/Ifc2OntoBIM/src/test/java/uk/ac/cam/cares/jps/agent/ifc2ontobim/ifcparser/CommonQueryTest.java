package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class CommonQueryTest {
    private static final String TEST_ZONE_VAR = "?zone";
    private static final String TEST_ZONE_VAR_REGEX = "\\" + TEST_ZONE_VAR;
    private static final String TEST_UID_VAR = "?uid";
    private static final String TEST_UID_VAR_REGEX = "\\" + TEST_UID_VAR;
    private static final String TEST_NAME_VAR = "?name";
    private static final String TEST_NAME_VAR_REGEX = "\\" + TEST_NAME_VAR;
    private static final String TEST_PLACEMENT_VAR = "?placement";
    private static final String TEST_PLACEMENT_VAR_REGEX = "\\" + TEST_PLACEMENT_VAR;

    @Test
    void testAddBaseQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addBaseQueryComponents(builder, TEST_ZONE_VAR, TEST_UID_VAR, TEST_NAME_VAR, TEST_PLACEMENT_VAR);
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

    @Test
    void testAddVoidRepresentationQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addVoidRepresentationQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedGeometricVoidQueryStatements(), builder.buildString());
    }

    @Test
    void testAddStairLandingQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addStairLandingQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedStairLandingQueryStatements(), builder.buildString());
    }

    @Test
    void testAddStairRailingQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addStairRailingQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedStairRailingQueryStatements(), builder.buildString());
    }

    @Test
    void testAddStairStructuralComponentQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addStairStructuralComponentQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedStairStructuralComponentQueryStatements(), builder.buildString());
    }

    @Test
    void testAddStairFlightQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addStairFlightQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedStairFlightQueryStatements(), builder.buildString());
    }

    private List<String> genExpectedBaseQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  " + TEST_ZONE_VAR_REGEX + " " + TEST_UID_VAR_REGEX + " " + TEST_NAME_VAR_REGEX + " " + TEST_PLACEMENT_VAR_REGEX);
        expected.add(TEST_ZONE_VAR_REGEX + " ifc:globalId_IfcRoot/express:hasString " + TEST_UID_VAR_REGEX + " .");
        expected.add(TEST_ZONE_VAR_REGEX + " ifc:name_IfcRoot/express:hasString " + TEST_NAME_VAR_REGEX + " .");
        expected.add(TEST_ZONE_VAR_REGEX + "     ifc:objectPlacement_IfcProduct  " + TEST_PLACEMENT_VAR_REGEX);
        expected.add(TEST_PLACEMENT_VAR_REGEX + "  rdf:type            ifc:IfcLocalPlacement}");
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

    private List<String> genExpectedGeometricVoidQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?openingelement \\?voidtype \\?voidplacement \\?voidshaperep \\?voidshapereptype \\?voidgeometry \\?voidsubcontext");
        expected.add("\\?relvoidelement\n" +
                "                  rdf:type              ifc:IfcRelVoidsElement ;\n" +
                "                  ifc:relatedOpeningElement_IfcRelVoidsElement  \\?openingelement ;\n" +
                "                  ifc:relatingBuildingElement_IfcRelVoidsElement  \\?element .");
        expected.add("\\?openingelement\n" +
                "                  rdf:type              ifc:IfcOpeningElement");
        expected.add("\\?openingelement ifc:objectType_IfcObject/express:hasString \\?voidtype");
        expected.add("\\?openingelement\n" +
                "                  ifc:objectPlacement_IfcProduct  \\?voidplacement");
        expected.add("\\?voidplacement\n" +
                "                  rdf:type              ifc:IfcLocalPlacement");
        expected.add("\\?openingelement\n" +
                "                  ifc:representation_IfcProduct  \\?voidproductDefinition");
        expected.add("\\?voidproductDefinition ifc:representations_IfcProductRepresentation/list:hasContents \\?voidshaperep");
        expected.add("\\?voidshaperep\n" +
                "                  rdf:type  ifc:IfcShapeRepresentation");
        expected.add("\\?voidshaperep ifc:representationType_IfcRepresentation/express:hasString \\?voidshapereptype");
        expected.add("\\?voidshaperep\n" +
                "                  ifc:contextOfItems_IfcRepresentation  \\?voidsubcontext");
        expected.add("\\?voidsubcontext\n" +
                "                  rdf:type              ifc:IfcGeometricRepresentationSubContext");
        expected.add("\\?voidshaperep\n" +
                "                  ifc:items_IfcRepresentation  \\?voidgeometry");
        return expected;
    }

    private List<String> genExpectedStairLandingQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?landing \\?stairlandingshaperep \\?stairlandingsubcontext \\?stairlandinggeometry \\?stairlandingshapereptype \\?stairlandinguid \\?stairlandingname \\?stairlandingplacement");
        expected.add("\\?landing ifc:globalId_IfcRoot/express:hasString \\?stairlandinguid");
        expected.add("\\?landing ifc:name_IfcRoot/express:hasString \\?stairlandingname");
        expected.add("\\?landing  ifc:objectPlacement_IfcProduct  \\?stairlandingplacement");
        expected.add("\\?stairlandingplacement\n" +
                "              rdf:type              ifc:IfcLocalPlacement");
        expected.add("\\?relaggstair  rdf:type          ifc:IfcRelAggregates ;\n" +
                "              ifc:relatingObject_IfcRelDecomposes  \\?element ;\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  \\?landing");
        expected.add("\\?landing  rdf:type              ifc:IfcSlab ;\n" +
                "              ifc:representation_IfcProduct  \\?stairlandingproductDefinition");
        expected.add("\\?stairlandingproductDefinition\n" +
                "              rdf:type              ifc:IfcProductDefinitionShape");
        expected.add("\\?stairlandingproductDefinition ifc:representations_IfcProductRepresentation/list:hasContents \\?stairlandingshaperep");
        expected.add("\\?stairlandingshaperep\n" +
                "              rdf:type  ifc:IfcShapeRepresentation");
        expected.add("\\?stairlandingshaperep ifc:representationType_IfcRepresentation/express:hasString \\?stairlandingshapereptype");
        expected.add("\\?stairlandingshaperep\n" +
                "              ifc:contextOfItems_IfcRepresentation  \\?stairlandingsubcontext");
        expected.add("\\?stairlandingsubcontext\n" +
                "              rdf:type              ifc:IfcGeometricRepresentationSubContext");
        expected.add("\\?stairlandingshaperep\n" +
                "              ifc:items_IfcRepresentation  \\?stairlandinggeometry");
        expected.add("\\?stairlandinggeometry\n" +
                "              rdf:type              \\?stairlandinggeomtype");
        return expected;
    }

    private List<String> genExpectedStairRailingQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?railing \\?stairrailingshaperep \\?stairrailingsubcontext \\?stairrailinggeometry \\?stairrailingshapereptype \\?stairrailinguid \\?stairrailingname \\?stairrailingplacement");
        expected.add("\\?railing ifc:globalId_IfcRoot/express:hasString \\?stairrailinguid");
        expected.add("\\?railing ifc:name_IfcRoot/express:hasString \\?stairrailingname");
        expected.add("\\?railing  ifc:objectPlacement_IfcProduct  \\?stairrailingplacement");
        expected.add("\\?stairrailingplacement\n" +
                "              rdf:type              ifc:IfcLocalPlacement");
        expected.add("\\?relaggstair  rdf:type          ifc:IfcRelAggregates ;\n" +
                "              ifc:relatingObject_IfcRelDecomposes  \\?element ;\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  \\?railing");
        expected.add("\\?railing  rdf:type              ifc:IfcRailing ;\n" +
                "              ifc:representation_IfcProduct  \\?stairrailingproductDefinition");
        expected.add("\\?stairrailingproductDefinition\n" +
                "              rdf:type              ifc:IfcProductDefinitionShape");
        expected.add("\\?stairrailingproductDefinition ifc:representations_IfcProductRepresentation/list:hasContents \\?stairrailingshaperep");
        expected.add("\\?stairrailingshaperep\n" +
                "              rdf:type  ifc:IfcShapeRepresentation");
        expected.add("\\?stairrailingshaperep ifc:representationType_IfcRepresentation/express:hasString \\?stairrailingshapereptype");
        expected.add("\\?stairrailingshaperep\n" +
                "              ifc:contextOfItems_IfcRepresentation  \\?stairrailingsubcontext");
        expected.add("\\?stairrailingsubcontext\n" +
                "              rdf:type              ifc:IfcGeometricRepresentationSubContext");
        expected.add("\\?stairrailingshaperep\n" +
                "              ifc:items_IfcRepresentation  \\?stairrailinggeometry");
        expected.add("\\?stairrailinggeometry\n" +
                "              rdf:type              \\?stairrailinggeomtype");
        return expected;
    }

    private List<String> genExpectedStairStructuralComponentQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?stairstructurecomponent \\?stairmembershaperep \\?stairmembersubcontext \\?stairmembergeometry \\?stairmembershapereptype \\?stairmemberuid \\?stairmembername \\?stairmemberplacement");
        expected.add("\\?stairstructurecomponent ifc:globalId_IfcRoot/express:hasString \\?stairmemberuid");
        expected.add("\\?stairstructurecomponent ifc:name_IfcRoot/express:hasString \\?stairmembername");
        expected.add("\\?stairstructurecomponent\n" +
                "              ifc:objectPlacement_IfcProduct  \\?stairmemberplacement");
        expected.add("\\?stairmemberplacement\n" +
                "              rdf:type              ifc:IfcLocalPlacement");
        expected.add("\\?relaggstair  rdf:type          ifc:IfcRelAggregates ;\n" +
                "              ifc:relatingObject_IfcRelDecomposes  \\?element ;\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  \\?stairstructurecomponent");
        expected.add("\\?stairstructurecomponent\n" +
                "              rdf:type              ifc:IfcMember ;\n" +
                "              ifc:representation_IfcProduct  \\?stairmemberproductDefinition");
        expected.add("\\?stairmemberproductDefinition\n" +
                "              rdf:type              ifc:IfcProductDefinitionShape");
        expected.add("\\?stairmemberproductDefinition ifc:representations_IfcProductRepresentation/list:hasContents \\?stairmembershaperep");
        expected.add("\\?stairmembershaperep\n" +
                "              rdf:type  ifc:IfcShapeRepresentation");
        expected.add("\\?stairmembershaperep ifc:representationType_IfcRepresentation/express:hasString \\?stairmembershapereptype");
        expected.add("\\?stairmembershaperep\n" +
                "              ifc:contextOfItems_IfcRepresentation  \\?stairmembersubcontext");
        expected.add("\\?stairmembersubcontext\n" +
                "              rdf:type              ifc:IfcGeometricRepresentationSubContext");
        expected.add("\\?stairmembershaperep\n" +
                "              ifc:items_IfcRepresentation  \\?stairmembergeometry");
        expected.add("\\?stairmembergeometry\n" +
                "              rdf:type              \\?stairmembergeomtyp");
        return expected;
    }

    private List<String> genExpectedStairFlightQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?stairflight \\?riserno \\?treadno \\?riserheight \\?treadlength \\?stairflightshaperep \\?stairflightsubcontext \\?stairflightgeometry \\?stairflightshapereptype \\?stairflightuid \\?stairflightname \\?stairflightplacement");
        expected.add("\\?stairflight ifc:globalId_IfcRoot/express:hasString \\?stairflightuid");
        expected.add("\\?stairflight ifc:name_IfcRoot/express:hasString \\?stairflightname");
        expected.add("\\?stairflight  ifc:objectPlacement_IfcProduct  \\?stairflightplacement");
        expected.add("\\?stairflightplacement\n" +
                "              rdf:type              ifc:IfcLocalPlacement");
        expected.add("\\?relaggstair  rdf:type          ifc:IfcRelAggregates ;\n" +
                "              ifc:relatingObject_IfcRelDecomposes  \\?element ;\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  \\?stairflight .");
        expected.add("\\?stairflight  rdf:type          ifc:IfcStairFlight");
        expected.add("\\?stairflight ifc:numberOfRiser_IfcStairFlight/express:hasInteger \\?riserno");
        expected.add("\\?stairflight ifc:numberOfTreads_IfcStairFlight/express:hasInteger \\?treadno");
        expected.add("\\?stairflight ifc:riserHeight_IfcStairFlight/express:hasDouble \\?riserheight");
        expected.add("\\?stairflight ifc:treadLength_IfcStairFlight/express:hasDouble \\?treadlength");
        expected.add("\\?stairflight  ifc:representation_IfcProduct  \\?stairflightproductDefinition");
        expected.add("\\?stairflightproductDefinition\n" +
                "              rdf:type              ifc:IfcProductDefinitionShape");
        expected.add("\\?stairflightproductDefinition ifc:representations_IfcProductRepresentation/list:hasContents \\?stairflightshaperep");
        expected.add("\\?stairflightshaperep\n" +
                "              rdf:type  ifc:IfcShapeRepresentation");
        expected.add("\\?stairflightshaperep ifc:representationType_IfcRepresentation/express:hasString \\?stairflightshapereptype");
        expected.add("\\?stairflightshaperep\n" +
                "              ifc:contextOfItems_IfcRepresentation  \\?stairflightsubcontext");
        expected.add("\\?stairflightsubcontext\n" +
                "              rdf:type              ifc:IfcGeometricRepresentationSubContext");
        expected.add("\\?stairflightshaperep\n" +
                "              ifc:items_IfcRepresentation  \\?stairflightgeometry");
        expected.add("\\?stairflightgeometry\n" +
                "              rdf:type              \\?stairflightgeomtype");
        return expected;
    }
}