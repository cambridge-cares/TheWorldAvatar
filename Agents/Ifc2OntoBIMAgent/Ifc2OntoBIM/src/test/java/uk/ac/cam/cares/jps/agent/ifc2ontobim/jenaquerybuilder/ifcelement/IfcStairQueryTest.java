package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcStairQueryTest {
    private static ConstructBuilder builder;

    @BeforeEach
    void initBuilder() {
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
    }

    @Test
    void testAddSubElementsQueryComponents() {
        IfcStairQuery.addSubElementsQueryComponents(builder);
        String query = builder.buildString();
        List<String> expected = this.genExpectedResults();
        expected.forEach(line -> assertTrue(query.contains(line)));
    }

    @Test
    void testAddSubElementsQueryComponentsFail() {
        ConstructBuilder failBuilder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> IfcStairQuery.addSubElementsQueryComponents(failBuilder));
        assertTrue(thrownError.getMessage().contains("Predicate"));
        assertTrue(thrownError.getMessage().contains("must be a Path, URI , variable, or a wildcard."));
    }

    private List<String> genExpectedResults() {
        List<String> expected = new ArrayList<>();
        // Construct statements
        expected.add("?element bot:hasSubElement ?stairflight");
        expected.add("?element bot:hasSubElement ?landing");
        expected.add("?element bot:hasSubElement ?railing");
        expected.add("?element bot:hasSubElement ?structurecomponent");
        expected.add("?stairflight rdf:type bim:StairFlight");
        expected.add("?landing rdf:type bim:Landing");
        expected.add("?railing rdf:type bim:Railing");
        expected.add("?structurecomponent rdf:type bim:StructuralComponent");
        expected.add("?stairflight bim:hasNumOfRiser ?riserno");
        expected.add("?stairflight bim:hasNumOfTread ?treadno");
        expected.add("?stairflight bim:hasRiserHeight ?riserheight");
        expected.add("?stairflight bim:hasTreadLength ?treadlength");
        expected.add("?stairflight bim:hasLocalPosition ?stairflightplacement");
        expected.add("?stairflightplacement rdf:type bim:LocalPlacement");
        expected.add("?stairflight bim:hasGeometricRepresentation ?stairflightshaperep");
        expected.add("?stairflightshaperep rdf:type bim:ModelRepresentation3D");
        expected.add("?stairflightshaperep bim:hasRepresentationType ?stairflightshapereptype");
        expected.add("?stairflightshaperep bim:hasSubContext ?stairflightcontext");
        expected.add("?stairflightcontext rdf:type bim:GeometricRepresentationSubContext");
        expected.add("?stairflightshaperep bim:hasRepresentationItem ?stairflightgeom");
        expected.add("?stairflightgeom rdf:type ?stairflightgeom");

        // Where statements
        expected.add("?relagg   rdf:type              ifc:IfcRelAggregates ;\n" +
                "              ifc:relatingObject_IfcRelDecomposes  ?element ;\n" +
                "              ifc:relatedObjects_IfcRelDecomposes  ?stairflight .\n" +
                "    ?stairflight  rdf:type          ifc:IfcStairFlight .\n" +
                "    ?relagg   ifc:relatedObjects_IfcRelDecomposes  ?landing .\n" +
                "    ?landing  rdf:type              ifc:IfcSlab .\n" +
                "    ?relagg   ifc:relatedObjects_IfcRelDecomposes  ?railing .\n" +
                "    ?railing  rdf:type              ifc:IfcRailing .\n" +
                "    ?relagg   ifc:relatedObjects_IfcRelDecomposes  ?structurecomponent .\n" +
                "    ?structurecomponent\n" +
                "              rdf:type              ifc:IfcMember");
        expected.add("?stairflight ifc:numberOfRiser_IfcStairFlight/express:hasInteger ?riserno");
        expected.add("?stairflight ifc:numberOfTreads_IfcStairFlight/express:hasInteger ?treadno");
        expected.add("?stairflight ifc:riserHeight_IfcStairFlight/express:hasDouble ?riserheight");
        expected.add("?stairflight ifc:treadLength_IfcStairFlight/express:hasDouble ?treadlength");
        expected.add("?stairflight  ifc:objectPlacement_IfcProduct  ?stairflightplacement ;\n" +
                "              ifc:representation_IfcProduct  ?stairflightdefinition .\n" +
                "    ?stairflightdefinition ifc:representations_IfcProductRepresentation/list:hasContents ?stairflightshaperep .\n" +
                "    ?stairflightshaperep\n" +
                "              rdf:type  ifc:IfcShapeRepresentation .\n" +
                "    ?stairflightshaperep ifc:representationType_IfcRepresentation/express:hasString ?stairflightshapereptype .\n" +
                "    ?stairflightshaperep\n" +
                "              ifc:contextOfItems_IfcRepresentation  ?stairflightcontext .\n" +
                "    ?stairflightcontext\n" +
                "              rdf:type              ifc:IfcGeometricRepresentationSubContext .\n" +
                "    ?stairflightshaperep\n" +
                "              ifc:items_IfcRepresentation  ?stairflightgeom .\n" +
                "    ?stairflightgeom\n" +
                "              rdf:type              ?stairflightgeomtype");
        return expected;
    }
}