package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class JunitTestUtils {
    public static final String bimUri = "http://www.theworldavatar.com/kg/ontobim/";
    public static final String botUri = "https://w3id.org/bot#";
    public static final String expressUri = "https://w3id.org/express#";
    public static final String ifc2x3Uri = "http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#";
    public static final String listUri = "https://w3id.org/list#";
    public static final String rdfUri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String rdfsUri = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String IFC2X3_ID_PROPERTY = ifc2x3Uri + "globalId_IfcRoot";
    public static final String IFC2X3_NAME_PROPERTY = ifc2x3Uri + "name_IfcRoot";
    public static final String IFC2X3_HOST_ZONE_PROPERTY = ifc2x3Uri + "relatingStructure_IfcRelContainedInSpatialStructure";
    public static final String IFC2X3_CONTAIN_ELEMENT_PROPERTY = ifc2x3Uri + "relatedElements_IfcRelContainedInSpatialStructure";
    public static final String BIM_PLACEMENT_CLASS = "LocalPlacement";
    public static final String IFC_PLACEMENT_CLASS = "IfcLocalPlacement";
    public static final String BIM_TRANSFORMATION_OPERATOR_CLASS = "CartesianTransformationOperator";
    public static final String IFC_TRANSFORMATION_OPERATOR_CLASS = "IfcCartesianTransformationOperator";
    public static final Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
    public static final Property hasBoolean = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasBoolean");
    public static final Property hasContents = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasContents");
    public static final Property hasNext = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasNext");

    public static void addPrefix(ConstructBuilder builder) {
        Map<String, String> nsMapping = new HashMap<>();
        nsMapping.put("rdfs", rdfsUri);
        nsMapping.put("rdf", rdfUri);
        nsMapping.put("bim", bimUri);
        nsMapping.put("bot", botUri);
        nsMapping.put("ifc", ifc2x3Uri);
        nsMapping.put("list", listUri);
        nsMapping.put("express", expressUri);
        builder.addPrefixes(nsMapping);
    }

    public static String appendStatementsAsString(LinkedHashSet<Statement> statementSet) {
        StringBuilder builder = new StringBuilder();
        statementSet.forEach(statement -> builder.append(statement.toString()));
        return builder.toString();
    }

    public static void doesExpectedListExist(List<String> expected, String result) {
        expected.forEach(statement -> {
            Matcher matcher = Pattern.compile(statement).matcher(result);
            assertTrue(matcher.find());
        });
    }

    public static void doesExpectedListNotExist(List<String> expected, String result) {
        expected.forEach(statement -> {
            Matcher matcher = Pattern.compile(statement).matcher(result);
            assertFalse(matcher.find());
        });
    }
}
