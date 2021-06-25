package uk.ac.cam.cares.jps.agent.spin.test;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.topbraid.spin.constraints.SPINConstraints;
import org.topbraid.spin.system.SPINModuleRegistry;
import uk.ac.cam.cares.jps.agent.spin.SpinChemical;

import java.io.File;
import java.net.URISyntaxException;

public class SpinChemicalTest {

    public String testTurtleFile;
    @Before
    public void setTurtleFile() throws URISyntaxException {
        testTurtleFile = new File(ClassLoader.getSystemResource("test_SPIN.ttl").toURI()).getAbsolutePath();
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
    public void testGetBaseModel() {
        SpinChemical spinChemical = new SpinChemical();
        Model model = spinChemical.getBaseModel(testTurtleFile);
        Assert.assertNotNull(model);
        // Test the presence of some instances after reading the model
        String phaseSystemPrefix = model.getNsPrefixURI("phase_system");
        Resource moleFraction = model.createResource(phaseSystemPrefix+"MoleFraction");
        Assert.assertTrue(model.containsResource(moleFraction));

        String systemPrefix = model.getNsPrefixURI("system");
        Resource scalarValue = model.createResource(systemPrefix+"ScalarValue");
        Assert.assertTrue(model.containsResource(scalarValue));
    }

    @Test
    public void testInferredTriples() {
        SpinChemical spinChemical = new SpinChemical();
        OntModel ontModel = ModelFactory.createOntologyModel(
                OntModelSpec.OWL_MEM, spinChemical.getBaseModel(testTurtleFile));
        Model inferrenceTriples = spinChemical.inferredTriples(ontModel);
        // TODO: no inferred triples?
        Assert.assertEquals(0, inferrenceTriples.size());
    }

    // TODO: Gives errors when run together with the other tests
    @Test
    public void testGetAllConstraintViolations() {
        SpinChemical spinChemical = new SpinChemical();
        OntModel ontModel = ModelFactory.createOntologyModel(
                OntModelSpec.OWL_MEM, spinChemical.getBaseModel(testTurtleFile));
        spinChemical.getAllConstraintViolations(ontModel);
    }

}
