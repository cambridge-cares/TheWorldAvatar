package uk.ac.cam.cares.jps.agent.spin.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import uk.ac.cam.cares.jps.agent.spin.SpinElectrical;

import java.io.File;
import java.net.URISyntaxException;

public class SpinElectricalTest {

    // Turtle file containing a small ontology with instances and SPIN constraints
    public String testTurtleFile;

    @Before
    public void setTurtleFile() throws URISyntaxException {
        // Based on the purchase example from topbraid
        testTurtleFile = new File(ClassLoader.getSystemResource("SPINtest.ttl").toURI()).getAbsolutePath();
    }

    @Test
    public void testNewSpinChemical() {
        SpinElectrical spinElectrical = null;
        try {
            spinElectrical = new SpinElectrical();
        } finally {
            Assert.assertNotNull(spinElectrical);
        }
    }

    @Test
    public void testGetBaseModel() {
        SpinElectrical spinElectrical = new SpinElectrical();
        Model model = spinElectrical.getBaseModel(testTurtleFile);
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
        SpinElectrical spinElectrical = new SpinElectrical();
        OntModel ontModel = ModelFactory.createOntologyModel(
                OntModelSpec.OWL_MEM, spinElectrical.getBaseModel(testTurtleFile));
        Model inferrenceTriples = spinElectrical.inferredTriples(ontModel);
        Assert.assertEquals(1, inferrenceTriples.size());
    }

}
