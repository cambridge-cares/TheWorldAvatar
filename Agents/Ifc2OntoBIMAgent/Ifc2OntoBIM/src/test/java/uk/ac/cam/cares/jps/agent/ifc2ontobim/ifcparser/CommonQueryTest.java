package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class CommonQueryTest {

    @Test
    void testAddBaseQueryComponents() {
        // Set up a new builder
        SelectBuilder builder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);
        // Execute method
        CommonQuery.addBaseQueryComponents(builder);
        // Test result
        JunitTestUtils.doesExpectedListExist(genExpectedQueryStatements(), builder.buildString());
    }

    private List<String> genExpectedQueryStatements() {
        List<String> expected = new ArrayList<>();
        expected.add("SELECT  \\?uid \\?name \\?placement");
        expected.add("\\?zone ifc:globalId_IfcRoot/express:hasString \\?uid .");
        expected.add("\\?zone ifc:name_IfcRoot/express:hasString \\?name .");
        expected.add("\\?zone     ifc:objectPlacement_IfcProduct  \\?placement .");
        expected.add("\\?placement  rdf:type            ifc:IfcLocalPlacement}");
        return expected;
    }
}