package uk.ac.cam.cares.jps.agent.spin.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import uk.ac.cam.cares.jps.agent.spin.SpinChemical;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.net.URISyntaxException;

public class SpinChemicalTest {

    // For testing System.out print statements
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    // Turtle file containing a small ontology with instances and SPIN constraints
    public String testTurtleFile;

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
    public void testQueryRDF4J() {
        // TODO: Can not be tested as the method requires a knowledge graph to run on the localhost for querying
        // SpinChemical spinChemical = new SpinChemical();
        // String csvFilePath = spinChemical.queryRDF4J("https://example.url", null);
        // Assert.assertNull(csvFilePath);
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
