package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * createDerivedQuantity, createDerivedQuantityWithTimeSeries, updateTimestamp, addTimeinstance
 * are already tested in DerivedQuantityClientTest
 * @author Kok Foong Lee
 * @author Jiaru Bai
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
    
	private String p_agent = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#";
	private String hasOperation = p_agent + "hasOperation";
	private String hasInput = p_agent + "hasInput";
	private String hasMandatoryPart = p_agent + "hasMandatoryPart";
	private String hasType = p_agent + "hasType";
	private String derivedAgentOperation = "http://derivedagent1/Operation";
	private String derivedAgentInputMsgCont = "http://derivedagent1/MsgContInput";
	private String derivedAgentMsgPart1 = "http://derivedagent1/InputMsgPart1";
	private String derivedAgentMsgPart2 = "http://derivedagent1/InputMsgPart2";
	private String input1RdfType = "http://input1/rdftype";
	private String input2RdfType = "http://input2/rdftype";
	private String input1ParentRdfType = "http://input1/parent_rdftype";
	private String input2ParentRdfType = "http://input2/parent_rdftype";

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
	public void testGetDerivationsOf() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		
		Map<String,String> derivationsOf = devClient.getDerivationsOf(entities);
		for (String entity : entities) {
			Assert.assertEquals(derivedIRI, derivationsOf.get(entity));
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
	public void testCreateDerivationAsyncForUpdate() {
		OntModel testKG = mockClient.getKnowledgeBase();
		boolean forUpdate = true;
		Resource derivationType = ResourceFactory.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");

		String derivationIRI = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, forUpdate);
		Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRI).getRDFType());

		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRI),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				ResourceFactory.createResource(derivedAgentIRI)));

		for (String entity : entities) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivationIRI)));
		}

		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRI),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(derivationIRI));
	}

	@Test
	public void testCreateDerivationAsyncForMarkup() {
		OntModel testKG = mockClient.getKnowledgeBase();
		boolean forUpdate = false;
		Resource derivationType = ResourceFactory.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");

		String derivationIRI = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, forUpdate);
		Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRI).getRDFType());

		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRI),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				ResourceFactory.createResource(derivedAgentIRI)));

		for (String entity : entities) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivationIRI)));
		}

		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRI),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		Assert.assertEquals(StatusType.NOSTATUS, devClient.getStatusType(derivationIRI));
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
	
	@Test
	public void testGetInputsMapToAgent() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// add triples about agent
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation), ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput), ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input1));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input2));
		
		// create asynchronous derivation
		boolean forUpdate = true;
		String derivationIRI = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, forUpdate);
		JSONObject mappedInputs = devClient.getInputsMapToAgent(derivationIRI, derivedAgentIRI);
		Assert.assertEquals(input1, mappedInputs.getString(input1));
		Assert.assertEquals(input2, mappedInputs.getString(input2));
	}

	@Test
	public void testGetInputsMapToAgent_RdfType_RdfsSubClassOf() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// add triples about agent
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation), ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput), ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input2ParentRdfType));
		
		// add triples about rdf:type and rdfs:subClassOf properties
		testKG.add(ResourceFactory.createResource(input1), RDF.type, ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(input1RdfType), RDFS.subClassOf, ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(input2), RDF.type, ResourceFactory.createResource(input2RdfType));
		testKG.add(ResourceFactory.createResource(input2RdfType), RDFS.subClassOf, ResourceFactory.createResource(input2ParentRdfType));

		// create asynchronous derivation
		boolean forUpdate = true;
		String derivationIRI = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, forUpdate);
		JSONObject mappedInputs = devClient.getInputsMapToAgent(derivationIRI, derivedAgentIRI);
		Assert.assertEquals(input1, mappedInputs.getString(input1ParentRdfType));
		Assert.assertEquals(input2, mappedInputs.getString(input2ParentRdfType));
	}

	@Test
	public void testGetInputsMapToAgent_RdfType() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// add triples about agent
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation), ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput), ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input2RdfType));
		
		// add triples about rdf:type and rdfs:subClassOf properties
		testKG.add(ResourceFactory.createResource(input1), RDF.type, ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(input2), RDF.type, ResourceFactory.createResource(input2RdfType));

		// create asynchronous derivation
		boolean forUpdate = true;
		String derivationIRI = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, forUpdate);
		JSONObject mappedInputs = devClient.getInputsMapToAgent(derivationIRI, derivedAgentIRI);
		Assert.assertEquals(input1, mappedInputs.getString(input1RdfType));
		Assert.assertEquals(input2, mappedInputs.getString(input2RdfType));
	}

	@Test
	public void testGetInputsMapToAgent_RdfsSubClassOf() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// add triples about agent
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation), ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput), ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input2ParentRdfType));
		
		// add triples about rdf:type and rdfs:subClassOf properties
		testKG.add(ResourceFactory.createResource(input1), RDFS.subClassOf, ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(input2), RDFS.subClassOf, ResourceFactory.createResource(input2ParentRdfType));

		// create asynchronous derivation
		boolean forUpdate = true;
		String derivationIRI = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, forUpdate);
		JSONObject mappedInputs = devClient.getInputsMapToAgent(derivationIRI, derivedAgentIRI);
		Assert.assertEquals(input1, mappedInputs.getString(input1ParentRdfType));
		Assert.assertEquals(input2, mappedInputs.getString(input2ParentRdfType));
	}

	@Test
	public void testRetrieveMatchingInstances() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// create first asynchronous derivation1
		boolean forUpdate = true;
		String derivationIRI = devClient.createDerivationAsync(Arrays.asList(entity1, entity2, entity3), derivedAgentIRI, inputs, forUpdate);

		// add triples about agent2 that monitors the derivation2 which is one derivation downstream compared to the derivation1
		// agent2 takes some entities from the output of the derivation1 as inputs
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation), ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput), ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont), ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType), ResourceFactory.createResource(input2ParentRdfType));
		
		// add triples about rdf:type and rdfs:subClassOf properties
		testKG.add(ResourceFactory.createResource(entity1), RDF.type, ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(input1RdfType), RDFS.subClassOf, ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(entity2), RDF.type, ResourceFactory.createResource(input2RdfType));
		testKG.add(ResourceFactory.createResource(input2RdfType), RDFS.subClassOf, ResourceFactory.createResource(input2ParentRdfType));

		// now we retrieve the output instances of the derivation1 that matches the input of derivation2 (OntoAgent I/O of agent2)
		List<String> mappedInstances = devClient.retrieveMatchingInstances(derivationIRI, derivedAgentIRI2);
		// the mappedInstances should be [entity1, entity2]
		mappedInstances.removeAll(Arrays.asList(entity1, entity2));
		Assert.assertTrue(mappedInstances.isEmpty());
	}

	@Test
	public void testGetDerivations() {
		List<List<String>> entitiesList = Arrays.asList(entities, entities2);
		List<List<String>> inputsList = Arrays.asList(inputs, inputs2);
				
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesList, agentIRIList, agentURLList, inputsList);
		devClient.addTimeInstance(derivationIRIs);
		
		List<Derivation> derivations = devClient.getDerivations();
		
		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI)).findFirst().get();
			
			List<Entity> inputs = derivation.getInputs();
			for (Entity input : inputs) {
				Assert.assertTrue(inputsList.get(i).contains(input.getIri()));
			}
			
			List<Entity> entities = derivation.getEntities();
			for (Entity entity : entities) {
				Assert.assertTrue(entitiesList.get(i).contains(entity.getIri()));
			}
			
			Assert.assertEquals(agentURLList.get(i), derivation.getAgentURL());
		}
	}
}
