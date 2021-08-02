package uk.ac.cam.cares.jps.base.derivedquantity;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.derivedquantity.DerivedQuantityClient;
import uk.ac.cam.cares.jps.base.derivedquantity.DerivedQuantitySparql;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * integration tests for updateInstance is provided at TheWorldAvater/Agents/DerivedAgent
 * @author Kok Foong Lee
 *
 */
public class DerivedQuantityClientTest{
	private MockDevStoreClient mockClient;
	private DerivedQuantityClient devClient;
	private String entity1 = "http://entity1"; 
	private String entity2 = "http://entity2"; 
    private List<String> entities = Arrays.asList(entity1,entity2);
    private String input1 = "http://input1"; 
    private String input2 = "http://input2"; 
    private List<String> inputs = Arrays.asList(input1,input2);
    private String derivedAgentIRI = "http://derivedagent1";
    private String derivedAgentURL = "http://localhost:8080/derivedagent1";
    private String derivedAgentIRI2 = "http://derivedagent2";
    private String derivedAgentURL2 = "http://localhost:8080/derivedagent2";
    private String derivedAgentIRI3 = "http://derivedagent3";
    private String derivedAgentURL3 = "http://localhost:8080/derivedagent3";
	
	@Before
    public void initialiseSparqlClient() {
        OntModel kb = ModelFactory.createOntologyModel();
        mockClient = new MockDevStoreClient(kb);
        devClient = new DerivedQuantityClient(mockClient);
    }
	
	@After
    public void closeKnowledgeBase() {
	    mockClient.closeKnowledgeBase();
    }
	
	@Test
    public void testConstructor() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteStoreClient kbClient = new RemoteStoreClient();    
    	DerivedQuantityClient client = new DerivedQuantityClient(kbClient);
    	// Retrieve the value of the private field 'kbClient' of the client
        Field kbc = client.getClass().getDeclaredField("kbClient");
        kbc.setAccessible(true);
        RemoteStoreClient kbcl = (RemoteStoreClient) kbc.get(client);
        // Test whether kbClients are the same 
    	Assert.assertSame(kbcl, kbClient);
    }
	
	@Test
	public void testKeys() {
		Assert.assertEquals("agent_input", DerivedQuantityClient.AGENT_INPUT_KEY);
		Assert.assertEquals("agent_output", DerivedQuantityClient.AGENT_OUTPUT_KEY);
	}
	
	@Test
	public void testCreateDerivedQuantity() {
		String createdDerived = devClient.createDerivedQuantity(entities, derivedAgentIRI, derivedAgentURL, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivedQuantitySparql.derivednamespace + "DerivedQuantity", devIndividual.getRDFType().toString()) ;
		
		// check that each entity is connected to the derived instance
		for (String entity : entities) {
		    Assert.assertTrue(testKG.contains(testKG.getIndividual(entity), 
		    		ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace+"belongsTo"),
		    		devIndividual));
		}
		
		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace+"isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));
		Assert.assertTrue(testKG.contains(testKG.getIndividual(derivedAgentIRI), 
				ResourceFactory.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl"),
				ResourceFactory.createResource(derivedAgentURL)));
		
		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace+"isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}
		
		// an instance cannot be part of two derived quantities
        try {
        	devClient.createDerivedQuantity(entities, derivedAgentIRI3, derivedAgentURL3, inputs);
        } catch (Exception e) {
        	Assert.assertTrue(e.getMessage().contains("part of another derived quantity"));
        }
	}
	
	@Test
	public void testCreateDerivedQuantityWithTimeSeries() {
		String createdDerived = devClient.createDerivedQuantityWithTimeSeries(entity1, derivedAgentIRI, derivedAgentURL, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivedQuantitySparql.derivednamespace + "DerivedQuantityWithTimeSeries", devIndividual.getRDFType().toString()) ;
		
		// check that entity is connected to the derived instance
	    Assert.assertTrue(testKG.contains(testKG.getIndividual(entity1), 
	    		ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace+"belongsTo"),
	    		devIndividual));
		
		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace+"isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));
		Assert.assertTrue(testKG.contains(testKG.getIndividual(derivedAgentIRI), 
				ResourceFactory.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl"),
				ResourceFactory.createResource(derivedAgentURL)));
		
		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace+"isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}
		
		// an instance cannot be part of two derived quantities
        try {
        	devClient.createDerivedQuantityWithTimeSeries(entity2, derivedAgentIRI3, derivedAgentURL3, inputs);
        } catch (Exception e) {
        	Assert.assertTrue(e.getMessage().contains("part of another derived quantity"));
        }
	}
	
	@Test
	public void testAddTimeInstance() {
		String namespace = "http://www.w3.org/2006/time#";
		devClient.addTimeInstance(input1);
		OntModel testKG = mockClient.getKnowledgeBase();
		RDFNode timeInstance = testKG.getIndividual(input1).getProperty(ResourceFactory.createProperty(namespace+"hasTime")).getObject();
		Assert.assertTrue(timeInstance.isResource());
		RDFNode timestamp = testKG.getIndividual(timeInstance.toString()).getProperty(ResourceFactory.createProperty(namespace+"numericPosition")).getObject();
		Assert.assertTrue(timestamp.isLiteral());
	}
	
	@Test
	public void testUpdateTimestamp() {
		String namespace = "http://www.w3.org/2006/time#";
		String devInstance = devClient.createDerivedQuantityWithTimeSeries(entity1, derivedAgentIRI, derivedAgentURL, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		long oldtime = testKG.getIndividual(devInstance).getProperty(ResourceFactory.createProperty(namespace+"hasTime")).getResource()
		.getProperty(ResourceFactory.createProperty(namespace+"numericPosition")).getLong();
		devClient.updateTimestamp(devInstance);
		long newtime = testKG.getIndividual(devInstance).getProperty(ResourceFactory.createProperty(namespace+"hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace+"numericPosition")).getLong();
		Assert.assertTrue(newtime > oldtime);
	}
	
	@Test
	public void testValidateDerived() {
		devClient.createDerivedQuantity(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		String derived2 = devClient.createDerivedQuantity(Arrays.asList(entity2), derivedAgentIRI2, derivedAgentURL2, Arrays.asList(entity1));
		
		// inputs do not have timestamps yet
		Assert.assertFalse(devClient.validateDerived(derived2));
		for (String input:inputs) {
			devClient.addTimeInstance(input);
		}
		
		Assert.assertTrue(devClient.validateDerived(derived2));
		
	    // intentionally create a circular dependency
		String derived3 = devClient.createDerivedQuantity(inputs, derivedAgentIRI3, derivedAgentURL3, Arrays.asList(entity1));
		Assert.assertFalse(devClient.validateDerived(derived3));
	}
}
