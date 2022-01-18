package uk.ac.cam.cares.jps.base.derivation;

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

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * integration tests for updateInstance is provided at TheWorldAvater/Agents/DerivationExample
 * @author Kok Foong Lee
 *
 */
public class DerivedQuantityClientTest{
	private MockDevStoreClient mockClient;
	private DerivationClient devClient;
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
        devClient = new DerivationClient(mockClient);
    }
	
	@After
    public void closeKnowledgeBase() {
	    mockClient.closeKnowledgeBase();
    }
	
	@Test
    public void testConstructor() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
    	RemoteStoreClient kbClient = new RemoteStoreClient();    
    	DerivationClient client = new DerivationClient(kbClient);
    	// Retrieve the value of the private field 'kbClient' of the client
        Field kbc = client.getClass().getDeclaredField("kbClient");
        kbc.setAccessible(true);
        RemoteStoreClient kbcl = (RemoteStoreClient) kbc.get(client);
        // Test whether kbClients are the same 
    	Assert.assertSame(kbcl, kbClient);
    }
	
	@Test
	public void testCreateDerivedQuantity() {
		String createdDerived = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "Derivation", devIndividual.getRDFType().toString()) ;
		
		// check that each entity is connected to the derived instance
		for (String entity : entities) {
		    Assert.assertTrue(testKG.contains(testKG.getIndividual(entity), 
		    		ResourceFactory.createProperty(DerivationSparql.derivednamespace+"belongsTo"),
		    		devIndividual));
		}
		
		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace+"isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));
		RDFNode operation = testKG.getIndividual(derivedAgentIRI).getProperty(ResourceFactory.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation")).getObject();
		RDFNode url = testKG.getIndividual(operation.toString()).getProperty(ResourceFactory.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl")).getObject();
        Assert.assertEquals(derivedAgentURL, url.toString());
        
		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace+"isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}
		
		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.createDerivation(entities, derivedAgentIRI3, derivedAgentURL3, inputs));
        Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}
	
	@Test
	public void testCreateDerivedQuantityWithTimeSeries() {
		String createdDerived = devClient.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "DerivationWithTimeSeries", devIndividual.getRDFType().toString()) ;
		
		// check that entity is connected to the derived instance
	    Assert.assertTrue(testKG.contains(testKG.getIndividual(entity1), 
	    		ResourceFactory.createProperty(DerivationSparql.derivednamespace+"belongsTo"),
	    		devIndividual));
		
		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace+"isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));
		RDFNode operation = testKG.getIndividual(derivedAgentIRI).getProperty(ResourceFactory.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation")).getObject();
		RDFNode url = testKG.getIndividual(operation.toString()).getProperty(ResourceFactory.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl")).getObject();
        Assert.assertEquals(derivedAgentURL, url.toString());
		
		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace+"isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}
		
		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI3, derivedAgentURL3, inputs));
        Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}
	
	@Test
	public void testAddTimeInstance() {
		String namespace = "http://www.w3.org/2006/time#";
		devClient.addTimeInstance(input1);
		OntModel testKG = mockClient.getKnowledgeBase();
		RDFNode timeInstance = testKG.getIndividual(input1).getProperty(ResourceFactory.createProperty(namespace+"hasTime")).getObject();
		Assert.assertTrue(timeInstance.isResource());
		RDFNode timeposition = testKG.getIndividual(timeInstance.toString()).getProperty(ResourceFactory.createProperty(namespace+"inTimePosition")).getObject();
		Assert.assertTrue(timeposition.isResource());
		RDFNode timestamp = testKG.getIndividual(timeposition.toString()).getProperty(ResourceFactory.createProperty(namespace+"numericPosition")).getObject();
		Assert.assertTrue(timestamp.isLiteral());
	}
	
	@Test
	public void testUpdateTimestamps() {
		String namespace = "http://www.w3.org/2006/time#";
		String devInstance = devClient.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		long oldtime = testKG.getIndividual(devInstance).getProperty(ResourceFactory.createProperty(namespace+"hasTime")).getResource()
		.getProperty(ResourceFactory.createProperty(namespace+"inTimePosition")).getResource()
		.getProperty(ResourceFactory.createProperty(namespace+"numericPosition")).getLong();
		devClient.updateTimestamps(Arrays.asList(entity1));
		long newtime = testKG.getIndividual(devInstance).getProperty(ResourceFactory.createProperty(namespace+"hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace+"inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace+"numericPosition")).getLong();
		Assert.assertTrue(newtime > oldtime);
	}
	
	@Test
	public void testValidateDerived() {
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2, derivedAgentURL2, Arrays.asList(entity1));
		
		// inputs do not have timestamps yet
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		for (String input:inputs) {
			devClient.addTimeInstance(input);
		}
		
		Assert.assertTrue(devClient.validateDerivations());
		
		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		
	    // intentionally create a circular dependency
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2, derivedAgentURL2, Arrays.asList(entity1));
		devClient.createDerivation(inputs, derivedAgentIRI3, derivedAgentURL3, Arrays.asList(entity1));
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("Edge would induce a cycle"));
		
		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		
		// pure inputs part of a derivation
		for (String input:inputs) {
			devClient.addTimeInstance(input);
		}
		devClient.createDerivation(inputs, derivedAgentIRI, derivedAgentURL, inputs);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("Entities belonging to a derivation should not have timestamps attached"));
	}
	
	@Test
	public void testDropDerivations() {
		OntModel testKG = mockClient.getKnowledgeBase();
		
		// case 1: standard derivation
		String derivation = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		
		// case 2: with time series
		derivation = devClient.createDerivationWithTimeSeries(entities, derivation, derivation, inputs);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		
		// case 3: both types present
		derivation = devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, Arrays.asList(input1));
		String derivation2 = devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI, derivedAgentURL, Arrays.asList(input2));
		Assert.assertNotNull(testKG.getIndividual(derivation));
		Assert.assertNotNull(testKG.getIndividual(derivation2));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		Assert.assertNull(testKG.getIndividual(derivation2));
	}
	
	@Test
	public void testDropTimestamps() {
		OntModel testKG = mockClient.getKnowledgeBase();

		for (String input : inputs) {
			devClient.addTimeInstance(input);
			Assert.assertNotNull(testKG.getIndividual(input));
		}
		
		devClient.dropAllTimestamps();
		
		for (String input : inputs) {
			Assert.assertNull(testKG.getIndividual(input));
		}
	}
}
