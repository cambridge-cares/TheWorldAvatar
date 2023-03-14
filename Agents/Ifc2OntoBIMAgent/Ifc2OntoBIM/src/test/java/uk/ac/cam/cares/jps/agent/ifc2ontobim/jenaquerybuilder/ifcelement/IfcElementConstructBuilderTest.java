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
    void testCreateSparqlQueryFail() {
        ConstructBuilder failBuilder = new ConstructBuilder();
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class,
                () -> new IfcElementConstructBuilder().createSparqlQuery(failBuilder, "ifc:IfcDoor", "bim:Door"));
        assertTrue(thrownError.getMessage().contains("Predicate"));
        assertTrue(thrownError.getMessage().contains("must be a Path, URI , variable, or a wildcard."));
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
}