package uk.ac.cam.cares.jps.agent.spin.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.matlab.JPSMatlabAgent;
import uk.ac.cam.cares.jps.agent.spin.SpinChemical;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

public class SpinChemicalTest {

    // For testing System.out print statements
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    // Turtle file containing a small ontology with instances and SPIN constraints
    public String testTurtleFile;

    // Folder for temporary files required by different tests that gets deleted after the tests
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Before
    public void setTurtleFile() throws URISyntaxException {
        // Based on the purchase example from topbraid
        testTurtleFile = new File(ClassLoader.getSystemResource("SPINtest.ttl").toURI()).getAbsolutePath();
    }

    @Before
    public void setUpStreams() {
        System.setOut(new PrintStream(outContent));
    }

    @After
    public void restoreStreams() {
        System.setOut(originalOut);
    }

    @Test
    public void testNewSpinChemical() {
        SpinChemical spinChemical = null;
        try {
            spinChemical = new SpinChemical();
        } finally {
            Assert.assertNotNull(spinChemical);
        }
    }

    @Test
    public void testQueryRDF4J() throws IOException {
        // Create a test file which path is returned by the mockup query result
        File file = folder.newFile("test.txt");
        // Absolute path to the file which is JSON safe
        String filePath = file.getAbsolutePath().replace(System.getProperty("file.separator"), "/");
        JPSMatlabAgent agent = new JPSMatlabAgent();
        String agentIRI = "http://test.com/agent";
        List<String> lst = new ArrayList<>();
        // Mock query result returned instead of actually querying for meta data (needs to be in SPARQL JSON return format)
        JSONObject mockedResult = new JSONObject();
        mockedResult.put("head", new JSONObject("{'vars': [ 'file']}"));
        mockedResult.put("results", new JSONObject("{'bindings': [ {'file': {'type': 'literal', 'value': '"+filePath+"'}} ]}"));
        try (MockedStatic<MetaDataQuery> theMock = Mockito.mockStatic(MetaDataQuery.class)) {
            theMock.when(() -> MetaDataQuery.queryResources(null, null, null, agentIRI, null, null, null, lst))
                    .thenReturn(mockedResult.toString());
            Assert.assertEquals(filePath, agent.queryRDF4J(agentIRI, lst));
        }
    }

    @Test
    public void testGetBaseModel() {
        SpinChemical spinChemical = new SpinChemical();
        Model model = spinChemical.getBaseModel(testTurtleFile);
        Assert.assertNotNull(model);
        // Test the presence of some instances after reading the model
        String prefix = model.getNsPrefixURI("");
        Resource purchase = model.createResource(prefix+"Purchase");
        Assert.assertTrue(model.containsResource(purchase));
        Resource exampleInstance = model.createResource(prefix+"purchase103");
        Assert.assertTrue(model.containsResource(exampleInstance));
    }

    @Test
    public void testInferredTriples() {
        SpinChemical spinChemical = new SpinChemical();
        OntModel ontModel = ModelFactory.createOntologyModel(
                OntModelSpec.OWL_MEM, spinChemical.getBaseModel(testTurtleFile));
        Model inferrenceTriples = spinChemical.inferredTriples(ontModel);
        Assert.assertEquals(1, inferrenceTriples.size());
    }

    @Test
    public void testGetInferredTriples() {
        SpinChemical spinChemical = new SpinChemical();
        OntModel ontModel = ModelFactory.createOntologyModel(
                OntModelSpec.OWL_MEM, spinChemical.getBaseModel(testTurtleFile));
        Model inferrenceTriples = spinChemical.inferredTriples(ontModel);
        spinChemical.getInferredTriples(inferrenceTriples);
        // Check that there was an inferred triple
        String consoleOutput = outContent.toString();
        Assert.assertTrue(consoleOutput.contains(" purchase101 , amountPerPiece , 50.0"));
    }

    @Test
    public void testGetAllConstraintViolations() {
        SpinChemical spinChemical = new SpinChemical();
        OntModel ontModel = ModelFactory.createOntologyModel(
                OntModelSpec.OWL_MEM, spinChemical.getBaseModel(testTurtleFile));
        spinChemical.getAllConstraintViolations(ontModel);
        // Test whether the constraints are correctly noticed and printed to the System.out
        String consoleOutput = outContent.toString();
        String[] consoleOutputLines = consoleOutput.split(System.lineSeparator());
        Assert.assertTrue(consoleOutput.contains("Material purchase is larger than 550."));
        Assert.assertTrue(consoleOutput.contains("Service contract is smaller than 150."));
        Assert.assertEquals("Constraint violations for :purchase103: 1", consoleOutputLines[consoleOutputLines.length-1]);
    }

}
