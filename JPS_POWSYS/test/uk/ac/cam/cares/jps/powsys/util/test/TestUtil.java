package uk.ac.cam.cares.jps.powsys.util.test;

import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.ModelFactory;
import org.junit.Test;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.apache.jena.ontology.OntModel;

import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.util.Util;

import static com.github.stefanbirkner.systemlambda.SystemLambda.withEnvironmentVariable;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;


public class TestUtil  {
    private OntModel genSampleModel() {
        OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        String NS = "http://example.com/ontology#";

        OntClass classA = model.createClass(NS + "ClassA");
        OntClass classB = model.createClass(NS + "ClassB");
        OntClass classC = model.createClass(NS + "ClassC");
        model.createClass(NS + "ClassD");

        classB.addSuperClass(classA);
        classC.addSuperClass(classA);

        return model;
    }
    @Test
    public void testGetResourceDir() {
        ENAgent agent = new ENAgent();
        Path resourcePath = Paths.get(Util.getResourceDir(agent));
        assertTrue(resourcePath.endsWith("JPS_POWSYS/res"));
    }
    @Test
    public void testQueryResult() {
        OntModel model = genSampleModel();
        String query =  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT ?x { ?x rdfs:subClassOf ?y } ";

        String[] expectedArr0 = {"http://example.com/ontology#ClassC"};
        String[] expectedArr1 = {"http://example.com/ontology#ClassB"};

        List<String[]> actual = Util.queryResult(model, query);

        assertEquals(2, actual.size());
        assertArrayEquals(expectedArr0, actual.get(0));
        assertArrayEquals(expectedArr1, actual.get(1));
    }

    @Test
    public void testGetGAMSLocation_EnvVarPresent() throws Exception {
        String loc = withEnvironmentVariable("GAMSDIR", "C:/GAMS;")
                .execute(Util::getGAMSLocation);
        assertEquals("C:/GAMS/gams.exe", loc);
    }

    @Test
    public void testGetGAMSLocation_EnvVarAbsent() throws Exception {
        String loc = withEnvironmentVariable("GAMSDIR", null)
                .execute(Util::getGAMSLocation);
        assertEquals("C:/GAMS/win64/26.1/gams.exe", loc);
    }

    @Test
    public void testGetGenEmission() {
        String query = "PREFIX  j2:   <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\n" +
                "PREFIX  technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>\n" +
                "PREFIX  j1:   <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>\n" +
                "PREFIX  j9:   <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>\n" +
                "\n" +
                "SELECT  ?entity ?V_Actual_CO2_Emission ?V_Design_CO2_Emission\n" +
                "WHERE\n" +
                "  { ?entity   a                     j1:PowerGenerator ;\n" +
                "              technical_system:realizes  ?generation .\n" +
                "    ?generation  j9:hasEmission     ?emission .\n" +
                "    ?emission  a                    j9:Actual_CO2_Emission ;\n" +
                "              j2:hasValue           ?valueemission .\n" +
                "    ?valueemission\n" +
                "              j2:numericalValue     ?V_Actual_CO2_Emission .\n" +
                "    ?generation  j9:hasEmission     ?v_emission .\n" +
                "    ?v_emission  a                  j9:CO2_emission ;\n" +
                "              j2:hasValue           ?valueemission_d .\n" +
                "    ?valueemission_d\n" +
                "              j2:numericalValue     ?V_Design_CO2_Emission}";

        String[] expected = query.split("\\s+");
        String[] actual = Util.getGenEmission().buildString().split("\\s+");

        assertArrayEquals(expected, actual);
    }
}
