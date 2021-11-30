package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * createDerivedQuantity, createDerivedQuantityWithTimeSeries, updateTimestamp, addTimeinstance
 * are already tested in DerivedQuantityClientTest
 * @author Kok Foong Lee
 *
 */
public class DerivedQuantitySparqlTest {
	private MockDevStoreClient mockClient;
	private DerivationSparql devClient;
	private String entity1 = "http://entity1"; 
	private String entity2 = "http://entity2"; 
	private String entity3 = "http://entity3";
    private List<String> entities = Arrays.asList(entity1,entity2);
    
    private String entity4 = "http://entity4"; 
	private String entity5 = "http://entity5"; 
	private List<String> entities2 = Arrays.asList(entity4,entity5);
    
    private String input1 = "http://input1"; 
    private String input2 = "http://input2"; 
    private List<String> inputs = Arrays.asList(input1,input2);
    
    private String input3 = "http://input3"; 
    private List<String> inputs2 = Arrays.asList(input3);
    
    private String derivedAgentIRI = "http://derivedagent1";
    private String derivedAgentURL = "http://localhost:8080/derivedagent1";
    private String derivedAgentIRI2 = "http://derivedagent2";
    private String derivedAgentURL2 = "http://localhost:8080/derivedagent2";
    
    private List<String> agentIRIList = Arrays.asList(derivedAgentIRI,derivedAgentIRI2);
    private List<String> agentURLList = Arrays.asList(derivedAgentURL,derivedAgentURL2);
    
	@Before
    public void initialiseSparqlClient() {
        OntModel kb = ModelFactory.createOntologyModel();
        mockClient = new MockDevStoreClient(kb);
        devClient = new DerivationSparql(mockClient);
    }
	
	@After
    public void closeKnowledgeBase() {
	    mockClient.closeKnowledgeBase();
    }
	
	@Test
	public void testHasBelongsTo() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		// empty kg
		Method hasBelongsTo = devClient.getClass().getDeclaredMethod("hasBelongsTo", String.class);
		hasBelongsTo.setAccessible(true);
		Assert.assertFalse((boolean) hasBelongsTo.invoke(devClient, entity1));
		
		// derived quantity created
		devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue((boolean) hasBelongsTo.invoke(devClient, entity1));
	}
	
	@Test
	public void testCheckInstanceExists () throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		// empty kg
		Method checkInstanceExists = devClient.getClass().getDeclaredMethod("checkInstanceExists", String.class);
	    checkInstanceExists.setAccessible(true);
	    Assert.assertFalse((boolean) checkInstanceExists.invoke(devClient, entity1));
	    
	    devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue((boolean) checkInstanceExists.invoke(devClient, entity1));
	}
	
	@Test
	public void testGetAgentUrl() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertEquals(derivedAgentURL, devClient.getAgentUrl(derivedIRI));
	}
	
	@Test
	public void testGetInputs() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		List<String> queriedInputs = devClient.getInputs(derivedIRI);
		
		for (String queriedInput : queriedInputs) {
			Assert.assertTrue(inputs.contains(queriedInput));
		}
	}
	
	@Test
	public void testGetInputsAndDerived() {
		// when an input is not a derived quantity
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue(devClient.getInputsAndDerived(derivedIRI).containsAll(inputs));
		
		// when an input is a derived quantity
		String derivedIRI2 = devClient.createDerivation(Arrays.asList(entity3), derivedAgentIRI, derivedAgentURL, entities);
		Assert.assertTrue(devClient.getInputsAndDerived(derivedIRI2).contains(derivedIRI));
	}
	
	@Test
	public void testGetDerivedIRI() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		
		for (String entity : entities) {
			Assert.assertEquals(derivedIRI, devClient.getDerivedIRI(entity));
		}
	}
	
	@Test
	public void testGetDerivedEntities() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue(devClient.getDerivedEntities(derivedIRI).containsAll(entities));
	}
	
	@Test
	public void testGetIsDerivedFromEntities() {
		// this function is used when a derived quantity is an input to another derived quantity, in this case, entities
		// are inputs to derivedIRI2. The rdftype is used to reconnect instances
		// derivedIRI2 depends on derivedIRI
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		String derivedIRI2 = devClient.createDerivation(Arrays.asList(entity3), derivedAgentIRI, derivedAgentURL, entities);
		
		OntModel testKG = mockClient.getKnowledgeBase();
		// add RDF types for entities
		for (String entity : entities) {
			testKG.getIndividual(entity).addRDFType(ResourceFactory.createResource(entity + "class"));
		}
		
		List<List<String>> queryResult = devClient.getIsDerivedFromEntities(entities);
		List<String> derivedList = queryResult.get(0);
		List<String> rdfTypeList = queryResult.get(1);
		
		for (int i = 0; i < derivedList.size(); i++) {
			Assert.assertEquals(entities.get(i) + "class", rdfTypeList.get(i));
			Assert.assertEquals(derivedIRI2, derivedList.get(i));
		}
	}
	
	@Test
	public void testDeleteInstances() {
		Property a = ResourceFactory.createProperty("http://a");
		Resource b = ResourceFactory.createResource("http://b");
		
		// only in subject
		OntModel testKG = mockClient.getKnowledgeBase();
		testKG.add(ResourceFactory.createResource(entity1), a, b);
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity1), a, b));
		devClient.deleteInstances(Arrays.asList(entity1));
		Assert.assertFalse(testKG.contains(ResourceFactory.createResource(entity1), a, b));
		
		// only in object
		testKG.add(b, a, ResourceFactory.createResource(entity1));
		Assert.assertTrue(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
		devClient.deleteInstances(Arrays.asList(entity1));
		Assert.assertFalse(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
		
		// in both subject and object
		testKG.add(ResourceFactory.createResource(entity1), a, b);
		testKG.add(b, a, ResourceFactory.createResource(entity1));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity1), a, b));
		Assert.assertTrue(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
		devClient.deleteInstances(Arrays.asList(entity1));
		Assert.assertFalse(testKG.contains(ResourceFactory.createResource(entity1), a, b));
		Assert.assertFalse(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
	}
	
	@Test
	public void testGetTimestamp() {
		// no time stamp yet
		Assert.assertThrows(JPSRuntimeException.class, () -> devClient.getTimestamp(input1));
		
		// timestamp attached directly to input
		devClient.addTimeInstance(input1);
		devClient.getTimestamp(input1);
		
		//time stamp of an instance linked to a derived quantity
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		devClient.addTimeInstance(derivedIRI);
		
		for (String entity : entities) {
			Assert.assertEquals(devClient.getTimestamp(derivedIRI), devClient.getTimestamp(entity));
		}
	}
	
	@Test
	public void testUpdateTimestamp() {
		// simply checks new time stamp is more recent
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		devClient.addTimeInstance(derivedIRI);
		// the derived instance is initialised with timestamp = 0
		long oldtime = devClient.getTimestamp(derivedIRI);
		devClient.updateTimeStamp(derivedIRI);
		long newtime = devClient.getTimestamp(derivedIRI);
		Assert.assertTrue(newtime > oldtime);
	}
	
	@Test
	public void testGetInstanceClass() {
		String entityclass = entity1+"class";
		
		OntModel testKG = mockClient.getKnowledgeBase();
		// returns an empty string if there is no rdf:type
		Assert.assertEquals("", devClient.getInstanceClass(Arrays.asList(entity1)).get(0));
		testKG.add(ResourceFactory.createResource(entity1), RDF.type, ResourceFactory.createResource(entityclass));
		
		Assert.assertEquals(entityclass, devClient.getInstanceClass(Arrays.asList(entity1)).get(0));		
	}
	
	@Test
	public void testReconnectInputToDerived() {
		devClient.reconnectInputToDerived(input1, input2);
		OntModel testKG = mockClient.getKnowledgeBase();
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(input2), 
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
				ResourceFactory.createResource(input1)));
	}
	
	@Test
	public void testAddNewEntitiesToDerived() {
		String derived = "http://derived";
		devClient.addNewEntitiesToDerived(derived, entities);
		OntModel testKG = mockClient.getKnowledgeBase();
		
		for (String entity : entities) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derived)));
		}
	}
	
	@Test
	public void testBulkCreateDerivations() {
		OntModel testKG = mockClient.getKnowledgeBase();
		List<List<String>> entitiesList = Arrays.asList(entities, entities2);
		List<List<String>> inputsList = Arrays.asList(inputs,inputs2);
		
		Resource derivationType = ResourceFactory.createResource(DerivationSparql.derivednamespace + "Derivation");
		
		List<String> derivations = devClient.bulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList);
		for (int i = 0; i < derivations.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);
			
			Assert.assertEquals(derivationType, testKG.getIndividual(derivations.get(i)).getRDFType());
			Assert.assertEquals(agentURLList.get(i), devClient.getAgentUrl(derivations.get(i)));
			
			for (String entity : entities) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
						ResourceFactory.createResource(derivations.get(i))));
			}
			
			for (String input : inputs) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivations.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
						ResourceFactory.createResource(input)));
			}
		}
	}
	
	@Test
	public void testBulkCreateDerivationsWithTimeSeries() {
		OntModel testKG = mockClient.getKnowledgeBase();
		List<List<String>> entitiesList = Arrays.asList(entities, entities2);
		List<List<String>> inputsList = Arrays.asList(inputs,inputs2);
		
		Resource derivationType = ResourceFactory.createResource(DerivationSparql.derivednamespace + "DerivationWithTimeSeries");
		
		List<String> derivations = devClient.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIRIList, agentURLList, inputsList);
		for (int i = 0; i < derivations.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);
			
			Assert.assertEquals(derivationType, testKG.getIndividual(derivations.get(i)).getRDFType());
			Assert.assertEquals(agentURLList.get(i), devClient.getAgentUrl(derivations.get(i)));
			
			for (String entity : entities) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
						ResourceFactory.createResource(derivations.get(i))));
			}
			
			for (String input : inputs) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivations.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
						ResourceFactory.createResource(input)));
			}
		}
	}
}
