package uk.ac.cam.cares.jps.base.derivedquantity;

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
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

/**
 * createDerivedQuantity, createDerivedQuantityWithTimeSeries, updateTimestamp, addTimeinstance
 * are already tested in DerivedQuantityClientTest
 * @author Kok Foong Lee
 *
 */
public class DerivedQuantitySparqlTest {
	private MockDevStoreClient mockClient;
	private String entity1 = "http://entity1"; 
	private String entity2 = "http://entity2"; 
	private String entity3 = "http://entity3";
    private List<String> entities = Arrays.asList(entity1,entity2);
    private String input1 = "http://input1"; 
    private String input2 = "http://input2"; 
    private List<String> inputs = Arrays.asList(input1,input2);
    private String derivedAgentIRI = "http://derivedagent1";
    private String derivedAgentURL = "http://localhost:8080/derivedagent1";
    private String derivedAgentIRI2 = "http://derivedagent2";
    private String derivedAgentURL2 = "http://localhost:8080/derivedagent2";
    
	@Before
    public void initialiseSparqlClient() {
        OntModel kb = ModelFactory.createOntologyModel();
        mockClient = new MockDevStoreClient(kb);
    }
	
	@After
    public void closeKnowledgeBase() {
	    mockClient.closeKnowledgeBase();
    }
	
	@Test
	public void testHasBelongsTo() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		// empty kg
		Method hasBelongsTo = DerivedQuantitySparql.class.getDeclaredMethod("hasBelongsTo", StoreClientInterface.class, String.class);
		hasBelongsTo.setAccessible(true);
		Assert.assertFalse((boolean) hasBelongsTo.invoke(DerivedQuantitySparql.class, mockClient, entity1));
		
		// derived quantity created
		DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue((boolean) hasBelongsTo.invoke(DerivedQuantitySparql.class, mockClient, entity1));
	}
	
	@Test
	public void testCheckInstanceExists () throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		// empty kg
		Method checkInstanceExists = DerivedQuantitySparql.class.getDeclaredMethod("checkInstanceExists", StoreClientInterface.class, String.class);
	    checkInstanceExists.setAccessible(true);
	    Assert.assertFalse((boolean) checkInstanceExists.invoke(DerivedQuantitySparql.class, mockClient, entity1));
	    
	    DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue((boolean) checkInstanceExists.invoke(DerivedQuantitySparql.class, mockClient, entity1));
	}
	
	@Test
	public void testGetAgentUrl() {
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertEquals(derivedAgentURL, DerivedQuantitySparql.getAgentUrl(mockClient, derivedIRI));
	}
	
	@Test
	public void testGetInputs() {
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		String[] queriedInputs = DerivedQuantitySparql.getInputs(mockClient, derivedIRI);
		
		for (String queriedInput : queriedInputs) {
			Assert.assertTrue(inputs.contains(queriedInput));
		}
	}
	
	@Test
	public void testGetInputsAndDerived() {
		// when an input is not a derived quantity
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue(DerivedQuantitySparql.getInputsAndDerived(mockClient, derivedIRI).containsAll(inputs));
		
		// when an input is a derived quantity
		String derivedIRI2 = DerivedQuantitySparql.createDerivedQuantity(mockClient, Arrays.asList(entity3), derivedAgentIRI, derivedAgentURL, entities);
		Assert.assertTrue(DerivedQuantitySparql.getInputsAndDerived(mockClient, derivedIRI2).contains(derivedIRI));
	}
	
	@Test
	public void testGetDerivedIRI() {
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		
		for (String entity : entities) {
			Assert.assertEquals(derivedIRI, DerivedQuantitySparql.getDerivedIRI(mockClient, entity));
		}
	}
	
	@Test
	public void testGetDerivedEntities() {
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue(Arrays.asList(DerivedQuantitySparql.getDerivedEntities(mockClient, derivedIRI)).containsAll(entities));
	}
	
	@Test
	public void testGetIsDerivedFromEntities() {
		// this function is used when a derived quantity is an input to another derived quantity, in this case, entities
		// are inputs to derivedIRI2. The rdftype is used to reconnect instances
		// derivedIRI2 depends on derivedIRI
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		String derivedIRI2 = DerivedQuantitySparql.createDerivedQuantity(mockClient, Arrays.asList(entity3), derivedAgentIRI, derivedAgentURL, entities);
		
		OntModel testKG = mockClient.getKnowledgeBase();
		// add RDF types for entities
		for (String entity : entities) {
			testKG.getIndividual(entity).addRDFType(ResourceFactory.createResource(entity + "class"));
		}
		
		String[] entitiesArray = new String[entities.size()];
		List<List<String>> queryResult = DerivedQuantitySparql.getIsDerivedFromEntities(mockClient, entities.toArray(entitiesArray));
		List<String> derivedList = queryResult.get(0);
		List<String> rdfTypeList = queryResult.get(1);
		
		for (int i = 0; i < derivedList.size(); i++) {
			Assert.assertEquals(entitiesArray[i] + "class", rdfTypeList.get(i));
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
		DerivedQuantitySparql.deleteInstances(mockClient, entity1);
		Assert.assertFalse(testKG.contains(ResourceFactory.createResource(entity1), a, b));
		
		// only in object
		testKG.add(b, a, ResourceFactory.createResource(entity1));
		Assert.assertTrue(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
		DerivedQuantitySparql.deleteInstances(mockClient, entity1);
		Assert.assertFalse(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
		
		// in both subject and object
		testKG.add(ResourceFactory.createResource(entity1), a, b);
		testKG.add(b, a, ResourceFactory.createResource(entity1));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity1), a, b));
		Assert.assertTrue(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
		DerivedQuantitySparql.deleteInstances(mockClient, entity1);
		Assert.assertFalse(testKG.contains(ResourceFactory.createResource(entity1), a, b));
		Assert.assertFalse(testKG.contains(b, a, ResourceFactory.createResource(entity1)));
	}
	
	@Test
	public void testGetTimestamp() {
		// no time stamp yet
		Assert.assertThrows(JPSRuntimeException.class, () -> DerivedQuantitySparql.getTimestamp(mockClient, input1));
		
		// timestamp attached directly to input
		DerivedQuantitySparql.addTimeInstance(mockClient, input1);
		DerivedQuantitySparql.getTimestamp(mockClient, input1);
		
		//time stamp of an instance linked to a derived quantity
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		
		for (String entity : entities) {
			Assert.assertEquals(DerivedQuantitySparql.getTimestamp(mockClient, derivedIRI), DerivedQuantitySparql.getTimestamp(mockClient, entity));
		}
	}
	
	@Test
	public void testUpdateTimestamp() {
		// simply checks new time stamp is more recent
		String derivedIRI = DerivedQuantitySparql.createDerivedQuantity(mockClient, entities, derivedAgentIRI, derivedAgentURL, inputs);
		// the derived instance is initialised with timestamp = 0
		long oldtime = DerivedQuantitySparql.getTimestamp(mockClient, derivedIRI);
		DerivedQuantitySparql.updateTimeStamp(mockClient, derivedIRI);
		long newtime = DerivedQuantitySparql.getTimestamp(mockClient, derivedIRI);
		Assert.assertTrue(newtime > oldtime);
	}
	
	@Test
	public void testGetInstanceClass() {
		String entityclass = entity1+"class";
		
		OntModel testKG = mockClient.getKnowledgeBase();
		// returns an empty string if there is no rdf:type
		Assert.assertEquals("", DerivedQuantitySparql.getInstanceClass(mockClient, entity1)[0]);
		testKG.add(ResourceFactory.createResource(entity1), RDF.type, ResourceFactory.createResource(entityclass));
		
		Assert.assertEquals(entityclass, DerivedQuantitySparql.getInstanceClass(mockClient, entity1)[0]);
		
		testKG.add(ResourceFactory.createResource(entity1), RDF.type, ResourceFactory.createResource(entityclass+"1"));
		
	}
	
	@Test
	public void testReconnectInputToDerived() {
		DerivedQuantitySparql.reconnectInputToDerived(mockClient, input1, input2);
		OntModel testKG = mockClient.getKnowledgeBase();
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(input2), 
				ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace + "isDerivedFrom"),
				ResourceFactory.createResource(input1)));
	}
	
	@Test
	public void testIsDerivedWithTimeSeries() {
		String derived1 = DerivedQuantitySparql.createDerivedQuantity(mockClient, Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, Arrays.asList(input1));
	    String derived2 = DerivedQuantitySparql.createDerivedQuantityWithTimeSeries(mockClient, entity2, derivedAgentIRI2, derivedAgentURL2, Arrays.asList(input2));
	    
	    Assert.assertFalse(DerivedQuantitySparql.isDerivedWithTimeSeries(mockClient, derived1));
	    Assert.assertTrue(DerivedQuantitySparql.isDerivedWithTimeSeries(mockClient, derived2));
	}
	
	@Test
	public void testAddNewEntitiesToDerived() {
		String derived = "http://derived";
		String[] entitiesArray = new String[entities.size()];
		entities.toArray(entitiesArray);
		DerivedQuantitySparql.addNewEntitiesToDerived(mockClient, derived, entitiesArray);
		OntModel testKG = mockClient.getKnowledgeBase();
		
		for (String entity : entitiesArray) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity),
					ResourceFactory.createProperty(DerivedQuantitySparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derived)));
		}
	}
}
