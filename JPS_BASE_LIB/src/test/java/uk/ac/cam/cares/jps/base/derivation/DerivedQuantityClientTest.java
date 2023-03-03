package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * integration tests for updateAllSyncDerivations is provided at
 * TheWorldAvater/Agents/DerivationExample
 * 
 * integration tests for updateMixedAsyncDerivation, updatePureSyncDerivations,
 * and unifiedUpdateDerivation is provided at
 * TheWorldAvater/Agents/DerivationAsynExample
 * 
 * unit tests for getAgentUrl, getDerivationsOf, getStatusType,
 * getNewDerivedIRI, getDerivations(String agentIRI),
 * getDerivationsAndStatusType, updateStatusAtJobCompletion,
 * checkImmediateUpstreamDerivation (wrapper function of method
 * getUpstreamDerivationsNeedUpdate in DerivationSparql) are provided in
 * DerivedQuantitySparqlTest.java
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivedQuantityClientTest {
	private MockDevStoreClient mockClient;
	private DerivationClient devClient;
	private final String derivationInstanceBaseURL = "http://derivationclient/test/";
	private String entity1 = "http://entity1";
	private String entity2 = "http://entity2";
	private List<String> entities = Arrays.asList(entity1, entity2);
	private String input1 = "http://input1";
	private String input2 = "http://input2";
	private List<String> inputs = Arrays.asList(input1, input2);
	private String derivedAgentIRI = "http://derivedagent1";
	private String derivedAgentURL = "http://localhost:8080/derivedagent1";
	private String derivedAgentIRI2 = "http://derivedagent2";
	private String derivedAgentURL2 = "http://localhost:8080/derivedagent2";
	private String derivedAgentIRI3 = "http://derivedagent3";
	private String derivedAgentURL3 = "http://localhost:8080/derivedagent3";

	private String entity3 = "http://entity3";
	private String entity4 = "http://entity4";
	private String entity5 = "http://entity5";
	private String p_agent = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#";
	private String p_time = "http://www.w3.org/2006/time#";
	private String OntoAgent_Service = p_agent + "Service";
	private String OntoAgent_Operation = p_agent + "Operation";
	private String OntoAgent_MessageContent = p_agent + "MessageContent";
	private String OntoAgent_MessagePart = p_agent + "MessagePart";
	private String hasOperation = p_agent + "hasOperation";
	private String hasHttpUrl = p_agent + "hasHttpUrl";
	private String hasInput = p_agent + "hasInput";
	private String hasOutput = p_agent + "hasOutput";
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
	private String entity1RdfType = "http://entity1/rdftype";
	private String entity2RdfType = "http://entity2/rdftype";
	private String derivedAgentOperation2 = "http://derivedagent2/Operation";
	private List<String> allInstances = Arrays.asList(input1, input2, entity1, entity2, entity3, entity4, entity5);

	@Before
	public void initialiseSparqlClient() {
		OntModel kb = ModelFactory.createOntologyModel();
		mockClient = new MockDevStoreClient(kb);
		devClient = new DerivationClient(mockClient, derivationInstanceBaseURL);
	}

	@After
	public void closeKnowledgeBase() {
		mockClient.closeKnowledgeBase();
	}

	@Test
	public void testConstructor() throws NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		RemoteStoreClient kbClient = new RemoteStoreClient();
		DerivationClient client = new DerivationClient(kbClient, derivationInstanceBaseURL);
		// Retrieve the value of the private field 'kbClient' of the client
		Field kbc = client.getClass().getDeclaredField("kbClient");
		kbc.setAccessible(true);
		RemoteStoreClient kbcl = (RemoteStoreClient) kbc.get(client);
		// Test whether kbClients are the same
		Assert.assertSame(kbcl, kbClient);
	}

	@Test
	public void testCreateDerivedQuantity() {
		String createdDerived = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "Derivation", devIndividual.getRDFType().toString());

		// check that each entity is connected to the derived instance
		for (String entity : entities) {
			Assert.assertTrue(testKG.contains(testKG.getIndividual(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					devIndividual));
		}

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.createDerivation(entities, derivedAgentIRI3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testCreateDerivedQuantityWithTimeSeries() {
		String createdDerived = devClient.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI,
				inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "DerivationWithTimeSeries",
				devIndividual.getRDFType().toString());

		// check that entity is connected to the derived instance
		Assert.assertTrue(testKG.contains(testKG.getIndividual(entity1),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
				devIndividual));

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient
				.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testCreateAsyncDerivationForUpdate() {
		boolean forUpdate = true;
		String createdDerived = devClient.createAsyncDerivation(entities, derivedAgentIRI, inputs, forUpdate);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "DerivationAsyn",
				devIndividual.getRDFType().toString());

		// check that each entity is connected to the derived instance
		for (String entity : entities) {
			Assert.assertTrue(testKG.contains(testKG.getIndividual(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					devIndividual));
		}

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// checks the status
		Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(createdDerived));

		// checks the timestamp should be 0
		Assert.assertEquals(0, devClient.sparqlClient.getTimestamp(createdDerived));

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.createDerivation(entities, derivedAgentIRI3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testCreateAsyncDerivationForNewInfo() {
		String createdDerived = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.ONTODERIVATION_DERIVATIONASYN,
				devIndividual.getRDFType().toString());

		// check that NO entity is connected to the derived instance
		Assert.assertTrue(devClient.sparqlClient.getDerivedEntities(createdDerived).isEmpty());

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// checks the status
		Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(createdDerived));

		// checks the timestamp should be 0
		Assert.assertEquals(0, devClient.sparqlClient.getTimestamp(createdDerived));
	}

	@Test
	public void testBulkCreateAsyncDerivationForNewInfo() {
		List<String> agentIRIs = Arrays.asList(derivedAgentIRI, derivedAgentIRI2);
		List<List<String>> agentInputs = Arrays.asList(entities, inputs);
		List<String> createdDerivedList = devClient.bulkCreateAsyncDerivationsForNewInfo(agentIRIs, agentInputs);
		OntModel testKG = mockClient.getKnowledgeBase();

		for (int i = 0; i < createdDerivedList.size(); i++) {
			String createdDerived = createdDerivedList.get(i);
			Individual devIndividual = testKG.getIndividual(createdDerived);
			Assert.assertNotNull(devIndividual);
			Assert.assertEquals(DerivationSparql.ONTODERIVATION_DERIVATIONASYN,
					devIndividual.getRDFType().toString());

			// check that NO entity is connected to the derived instance
			Assert.assertTrue(devClient.sparqlClient.getDerivedEntities(createdDerived).isEmpty());

			// checks for agent
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
					testKG.getIndividual(agentIRIs.get(i))));

			// checks for inputs
			for (String input : agentInputs.get(i)) {
				Assert.assertTrue(testKG.contains(devIndividual,
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
						ResourceFactory.createResource(input)));
			}

			// checks the status
			Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(createdDerived));

			// checks the timestamp should be 0
			Assert.assertEquals(0, devClient.sparqlClient.getTimestamp(createdDerived));
		}
	}

	@Test
	public void testCreateAsyncDerivationForMarkup() {
		boolean forUpdate = false;
		String createdDerived = devClient.createAsyncDerivation(entities, derivedAgentIRI, inputs, forUpdate);
		OntModel testKG = mockClient.getKnowledgeBase();
		Individual devIndividual = testKG.getIndividual(createdDerived);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "DerivationAsyn",
				devIndividual.getRDFType().toString());

		// check that each entity is connected to the derived instance
		for (String entity : entities) {
			Assert.assertTrue(testKG.contains(testKG.getIndividual(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					devIndividual));
		}

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// checks the status
		Assert.assertEquals(StatusType.NOSTATUS, devClient.getStatusType(createdDerived));

		// checks the timestamp should be current timestamp (>0)
		Assert.assertTrue(devClient.sparqlClient.getTimestamp(createdDerived) > 0);

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.createDerivation(entities, derivedAgentIRI3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testCreateAsyncDerivation_FromExistingDerivation_ForUpdate() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// create first asynchronous derivation1
		boolean forUpdate = true;
		String upstreamDerivationIRI = devClient.createAsyncDerivation(Arrays.asList(entity1, entity2, entity3),
				derivedAgentIRI, inputs, forUpdate);

		// add triples about agent2 that monitors the derivation2 which is one
		// derivation downstream compared to the derivation1
		// agent2 takes some entities from the output of the derivation1 as inputs
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput),
				ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input2ParentRdfType));

		// add triples about rdf:type and rdfs:subClassOf properties
		testKG.add(ResourceFactory.createResource(entity1), RDF.type, ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(input1RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(entity2), RDF.type, ResourceFactory.createResource(input2RdfType));
		testKG.add(ResourceFactory.createResource(input2RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input2ParentRdfType));

		// now we create the second derivation given the upstream derivation
		String downstreamDerivationIRI = devClient.createAsyncDerivation(Arrays.asList(entity4, entity5),
				derivedAgentIRI2, upstreamDerivationIRI, forUpdate);

		Individual devIndividual = testKG.getIndividual(downstreamDerivationIRI);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "DerivationAsyn",
				devIndividual.getRDFType().toString());

		// check that each entity is connected to the derived instance
		for (String entity : Arrays.asList(entity4, entity5)) {
			Assert.assertTrue(testKG.contains(testKG.getIndividual(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					devIndividual));
		}

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI2)));

		// checks for inputs
		for (String input : Arrays.asList(entity1, entity2)) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// checks the status
		Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(downstreamDerivationIRI));

		// checks the timestamp should be 0
		Assert.assertEquals(0, devClient.sparqlClient.getTimestamp(downstreamDerivationIRI));

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient
				.createDerivation(Arrays.asList(entity4, entity5), derivedAgentIRI3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testCreateAsyncDerivation_FromExistingDerivation_ForNewInfo() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// create first asynchronous derivation1
		String upstreamDerivationIRI = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, inputs);

		// add triples about agent2 that monitors the derivation2 which is one
		// derivation downstream compared to the derivation1
		// agent2 takes some entities from the output of the derivation1 as inputs
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput),
				ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input2ParentRdfType));

		// add triples about rdf:type and rdfs:subClassOf properties
		testKG.add(ResourceFactory.createResource(entity1), RDF.type, ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(input1RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(entity2), RDF.type, ResourceFactory.createResource(input2RdfType));
		testKG.add(ResourceFactory.createResource(input2RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input2ParentRdfType));

		// now we create the second derivation given the upstream derivation
		List<String> inputsOfDownstreamDerivation = Arrays.asList(input1, input2, upstreamDerivationIRI);
		String downstreamDerivationIRI = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, inputsOfDownstreamDerivation);

		Individual devIndividual1 = testKG.getIndividual(upstreamDerivationIRI);
		Individual devIndividual2 = testKG.getIndividual(downstreamDerivationIRI);
		Assert.assertNotNull(devIndividual1);
		Assert.assertNotNull(devIndividual2);
		Assert.assertEquals(DerivationSparql.ONTODERIVATION_DERIVATIONASYN,
				devIndividual1.getRDFType().toString());
		Assert.assertEquals(DerivationSparql.ONTODERIVATION_DERIVATIONASYN,
				devIndividual2.getRDFType().toString());

		// check that NO entity is connected to both derived instances
		Assert.assertTrue(devClient.sparqlClient.getDerivedEntities(upstreamDerivationIRI).isEmpty());
		Assert.assertTrue(devClient.sparqlClient.getDerivedEntities(downstreamDerivationIRI).isEmpty());

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual1,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI)));
		Assert.assertTrue(testKG.contains(devIndividual2,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI2)));

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual1,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}
		for (String input : inputsOfDownstreamDerivation) {
			Assert.assertTrue(testKG.contains(devIndividual2,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// checks the status
		Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(upstreamDerivationIRI));
		Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(downstreamDerivationIRI));

		// checks the timestamp should be 0
		Assert.assertEquals(0, devClient.sparqlClient.getTimestamp(upstreamDerivationIRI));
		Assert.assertEquals(0, devClient.sparqlClient.getTimestamp(downstreamDerivationIRI));
	}

	@Test
	public void testCreateAsyncDerivation_FromExistingDerivation_ForMarkup() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// create first asynchronous derivation1
		boolean forUpdate = false;
		String upstreamDerivationIRI = devClient.createAsyncDerivation(Arrays.asList(entity1, entity2, entity3),
				derivedAgentIRI, inputs, forUpdate);

		// add triples about agent2 that monitors the derivation2 which is one
		// derivation downstream compared to the derivation1
		// agent2 takes some entities from the output of the derivation1 as inputs
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput),
				ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input2ParentRdfType));

		// add triples about rdf:type and rdfs:subClassOf properties
		testKG.add(ResourceFactory.createResource(entity1), RDF.type, ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(input1RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(entity2), RDF.type, ResourceFactory.createResource(input2RdfType));
		testKG.add(ResourceFactory.createResource(input2RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input2ParentRdfType));

		// now we create the second derivation given the upstream derivation
		String downstreamDerivationIRI = devClient.createAsyncDerivation(Arrays.asList(entity4, entity5),
				derivedAgentIRI2, upstreamDerivationIRI, forUpdate);

		Individual devIndividual = testKG.getIndividual(downstreamDerivationIRI);
		Assert.assertNotNull(devIndividual);
		Assert.assertEquals(DerivationSparql.derivednamespace + "DerivationAsyn",
				devIndividual.getRDFType().toString());

		// check that each entity is connected to the derived instance
		for (String entity : Arrays.asList(entity4, entity5)) {
			Assert.assertTrue(testKG.contains(testKG.getIndividual(entity),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					devIndividual));
		}

		// checks for agent
		Assert.assertTrue(testKG.contains(devIndividual,
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				testKG.getIndividual(derivedAgentIRI2)));

		// checks for inputs
		for (String input : Arrays.asList(entity1, entity2)) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// checks the status
		Assert.assertEquals(StatusType.NOSTATUS, devClient.getStatusType(downstreamDerivationIRI));

		// checks the timestamp should be current timestamp (>0)
		Assert.assertTrue(devClient.sparqlClient.getTimestamp(downstreamDerivationIRI) > 0);

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient
				.createDerivation(Arrays.asList(entity4, entity5), derivedAgentIRI3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testAddTimeInstance() {
		// both addTimeInstance(String) and addTimeInstance(List<String>) are tested here
		// we test four aspect of the method:
		// (1) do nothing if timestamp exist for instance already
		// (2) do nothing if the instance is derived data
		// (3) add time instance only if the instance is NOT derived data and do NOT have timestamp already
		// (4) only one timestamp added to the duplicated instances in the input argument

		// first test the case where the timestamp is not in the triple store and we add it
		String namespace = "http://www.w3.org/2006/time#";
		devClient.addTimeInstance(input1);
		OntModel testKG = mockClient.getKnowledgeBase();
		RDFNode timeInstance = testKG.getIndividual(input1)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getObject();
		Assert.assertTrue(timeInstance.isResource());
		RDFNode timeposition = testKG.getIndividual(timeInstance.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getObject();
		Assert.assertTrue(timeposition.isResource());
		RDFNode timestamp = testKG.getIndividual(timeposition.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getObject();
		Assert.assertTrue(timestamp.isLiteral());

		// create a derivation instance to make derived data exist in the triple store
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, Arrays.asList(input1));
		// the output entity1 should NOT have timestamp
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(entity1),
				ResourceFactory.createProperty(namespace + "hasTime")));
		// if add time instance to entity1 explicitly, still nothing should happen
		devClient.addTimeInstance(entity1);
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(entity1),
				ResourceFactory.createProperty(namespace + "hasTime")));

		// then try to add timestamp to a list of entities, including the first one and the derived data
		// check that nothing happens to the instance if its timestamp already exists
		// also nothing happens to the derived data
		// the other instances should have their timestamp added
		devClient.addTimeInstance(Arrays.asList(input1, input2, entity1));
		// check that the timestamp of the first instance is not changed
		JSONArray resultsForInput1 = mockClient.executeQuery(String.format(
			"SELECT * WHERE { <%s> <%s> ?timeInstance. ?timeInstance <%s> ?timePosition. ?timePosition <%s> ?timestamp. }",
			input1, namespace + "hasTime", namespace + "inTimePosition", namespace + "numericPosition"));
		Assert.assertEquals(1, resultsForInput1.length());
		Assert.assertEquals(timeInstance.toString(), resultsForInput1.getJSONObject(0).get("timeInstance"));
		Assert.assertEquals(timeposition.toString(), resultsForInput1.getJSONObject(0).get("timePosition"));
		Assert.assertEquals(timestamp.asLiteral().getLong(), resultsForInput1.getJSONObject(0).getLong("timestamp"));
		// check that the timestamp of the second instance is added
		RDFNode timeInstance2 = testKG.getIndividual(input2)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getObject();
		Assert.assertTrue(timeInstance2.isResource());
		RDFNode timeposition2 = testKG.getIndividual(timeInstance2.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getObject();
		Assert.assertTrue(timeposition2.isResource());
		RDFNode timestamp2 = testKG.getIndividual(timeposition2.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getObject();
		Assert.assertTrue(timestamp2.isLiteral());
		JSONArray resultsForInput2 = mockClient.executeQuery(String.format(
			"SELECT * WHERE { <%s> <%s> ?timeInstance. ?timeInstance <%s> ?timePosition. ?timePosition <%s> ?timestamp. }",
			input2, namespace + "hasTime", namespace + "inTimePosition", namespace + "numericPosition"));
		Assert.assertEquals(1, resultsForInput2.length());
		Assert.assertEquals(timeInstance2.toString(), resultsForInput2.getJSONObject(0).get("timeInstance"));
		Assert.assertEquals(timeposition2.toString(), resultsForInput2.getJSONObject(0).get("timePosition"));
		Assert.assertEquals(timestamp2.asLiteral().getLong(), resultsForInput2.getJSONObject(0).getLong("timestamp"));
		// check that the derived data doesn't have time instance
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(entity1),
				ResourceFactory.createProperty(namespace + "hasTime")));

		// only one timestamp is added to the duplicated instance in input arguments
		devClient.addTimeInstance(Arrays.asList(entity3, entity3, entity3));
		// check that only one instance of timestamp is added to entity3
		RDFNode timeInstance3 = testKG.getIndividual(entity3)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getObject();
		Assert.assertTrue(timeInstance3.isResource());
		RDFNode timeposition3 = testKG.getIndividual(timeInstance3.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getObject();
		Assert.assertTrue(timeposition3.isResource());
		RDFNode timestamp3 = testKG.getIndividual(timeposition3.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getObject();
		Assert.assertTrue(timestamp3.isLiteral());
		JSONArray resultsForEntity3 = mockClient.executeQuery(String.format(
			"SELECT * WHERE { <%s> <%s> ?timeInstance. ?timeInstance <%s> ?timePosition. ?timePosition <%s> ?timestamp. }",
			entity3, namespace + "hasTime", namespace + "inTimePosition", namespace + "numericPosition"));
		Assert.assertEquals(1, resultsForEntity3.length());
		Assert.assertEquals(timeInstance3.toString(), resultsForEntity3.getJSONObject(0).get("timeInstance"));
		Assert.assertEquals(timeposition3.toString(), resultsForEntity3.getJSONObject(0).get("timePosition"));
		Assert.assertEquals(timestamp3.asLiteral().getLong(), resultsForEntity3.getJSONObject(0).getLong("timestamp"));
	}

	@Test
	public void testAddTimeInstanceCurrentTimestamp() {
		long currentTs = Instant.now().getEpochSecond();
		String namespace = "http://www.w3.org/2006/time#";
		// first add time instance to input1 with 0 as default
		devClient.addTimeInstance(input1);
		// then add timestamp to both input1 and input2
		// nothing should happen to input1, but input2 will be added current timestamp
		devClient.addTimeInstanceCurrentTimestamp(Arrays.asList(input1, input2));
		OntModel testKG = mockClient.getKnowledgeBase();

		// check timestamp for input1, should be 0
		RDFNode timeInstance = testKG.getIndividual(input1)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getObject();
		Assert.assertTrue(timeInstance.isResource());
		RDFNode timeposition = testKG.getIndividual(timeInstance.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getObject();
		Assert.assertTrue(timeposition.isResource());
		RDFNode timestamp = testKG.getIndividual(timeposition.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getObject();
		Assert.assertTrue(timestamp.isLiteral());
		Assert.assertEquals(0, timestamp.asLiteral().getLong());

		// check timestamp for input2, should no less than currentTs
		RDFNode timeInstance2 = testKG.getIndividual(input2)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getObject();
		Assert.assertTrue(timeInstance2.isResource());
		RDFNode timeposition2 = testKG.getIndividual(timeInstance2.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getObject();
		Assert.assertTrue(timeposition2.isResource());
		RDFNode timestamp2 = testKG.getIndividual(timeposition2.toString())
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getObject();
		Assert.assertTrue(timestamp2.isLiteral());
		Assert.assertTrue(timestamp2.asLiteral().getLong() >= currentTs);
	}

	@Test
	public void testUpdateTimestamps() throws InterruptedException {
		String namespace = "http://www.w3.org/2006/time#";
		// create derivation, the timestamp of the pure inputs will be added automatically
		String devInstance = devClient.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI,
				Arrays.asList(input1, input2));
		OntModel testKG = mockClient.getKnowledgeBase();
		long oldtimeDevInstance = testKG.getIndividual(devInstance)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		long oldtimeInput1 = testKG.getIndividual(input1)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		long oldtimeInput2 = testKG.getIndividual(input2)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();

		// sleep for one sec, so that ensure the new timestamp to be added by the updateTimestamps is greater
		TimeUnit.SECONDS.sleep(1);

		// update the timestamp, this tests five folds of the function:
		// 1. update the timestamp of the derivation instance given the IRI of the output - newtime > oldtime for devInstance
		devClient.updateTimestamps(Arrays.asList(devInstance, entity1, entity2, derivedAgentIRI, input1, input2));
		long newtimeDevInstance = testKG.getIndividual(devInstance)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		Assert.assertTrue(newtimeDevInstance > oldtimeDevInstance);
		// 2. no timestamp is added to the output instance
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(entity1),
				ResourceFactory.createProperty(namespace + "hasTime")));
		// 3. update the timestamp of the pure input given its IRI - newtime > oldtime for both inputs
		long newtimeInput1 = testKG.getIndividual(input1)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		Assert.assertTrue(newtimeInput1 > oldtimeInput1);
		long newtimeInput2 = testKG.getIndividual(input2)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		Assert.assertTrue(newtimeInput2 > oldtimeInput2);
		// 4. do nothing for the non-exist IRI - entity2 is not in the triple store
		Assert.assertNull(testKG.getIndividual(entity2));
		// 5. do nothing for the IRI that doesn't have a timestamp accociated with it - no timestamp instances exist for derivedAgentIRI
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivedAgentIRI),
				ResourceFactory.createProperty(namespace + "hasTime")));

		// 6. update again with empty list - the method should do nothing if no IRI is given
		// sleep for one sec, so that ensure the new timestamp to be used by the updateTimestamps is greater if anything happens at all
		TimeUnit.SECONDS.sleep(1);
		devClient.updateTimestamps(new ArrayList<String>());
		long newtime = testKG.getIndividual(devInstance)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		Assert.assertEquals(newtime, newtimeDevInstance);
	}

	@Test
	public void testValidateDerived() {
		// initialise rdf:type of all instances, so that derivations can be cached
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);
		// also need to initialise ontoagent instance for agents, so that derivations can be cached
		devClient.createOntoAgentInstance(derivedAgentIRI, derivedAgentURL, Arrays.asList("http://i_1"), Arrays.asList("http://o_1"));
		devClient.createOntoAgentInstance(derivedAgentIRI2, derivedAgentURL2, Arrays.asList("http://i_2"), Arrays.asList("http://o_2"));
		devClient.createOntoAgentInstance(derivedAgentIRI3, derivedAgentURL3, Arrays.asList("http://i_3"), Arrays.asList("http://o_3"));

		// 1. if any pure inputs don't have timestamp - in theory this should not be possible, unless someone deleted manually
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, inputs);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2, Arrays.asList(entity1));
		// the first check should pass as the timestamp for pure inputs are added automatically
		Assert.assertTrue(devClient.validateDerivations());

		// now delete timestamp of one pure input, and the derivations should not be valid anymore
		mockClient.executeUpdate(String.format("delete where {<%s> <%s> ?time.}",
				inputs.get(0), "http://www.w3.org/2006/time#hasTime"));
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 2. intentionally create a circular dependency
		// case i: e1 --> i1, i2. i1 --> e1.
		// if we use the DerivationSparql::bulkCreateDerivations to bulk create
		// it will be successful, but will fail at validate
		List<String> derivations = devClient.sparqlClient.bulkCreateDerivations(Arrays.asList(Arrays.asList(entity1), Arrays.asList(input1)),
				Arrays.asList(derivedAgentIRI, derivedAgentIRI2),
				Arrays.asList(Arrays.asList(input1, input2), Arrays.asList(entity1)));
		devClient.addTimeInstance(derivations);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("Circular dependency likely occurred"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// case ii: e2 --> e1. e1 --> i1, i2. i1 --> e1.
		// if we use the DerivationSparql::bulkCreateDerivations to bulk create
		// it will be successful, but will fail at validate
		derivations = devClient.sparqlClient.bulkCreateDerivations(Arrays.asList(Arrays.asList(entity1), Arrays.asList(entity2), Arrays.asList(input1)),
				Arrays.asList(derivedAgentIRI, derivedAgentIRI2, derivedAgentIRI3),
				Arrays.asList(Arrays.asList(input1, input2), Arrays.asList(entity1), Arrays.asList(entity1)));
		devClient.addTimeInstance(derivations);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("Edge would induce a cycle"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 3. pure inputs part of a derivation
		// with the latest design, this should not be possible (nothing happens when trying to add timestamp to derived data)
		// but here we just create the situation manually
		String namespace = "http://www.w3.org/2006/time#";
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, inputs);
		mockClient.executeUpdate(
			String.format(
				"insert data {" +
					"<%s> <%s> <http://time_instant>." +
					"<http://time_instant> a <%s>; <%s> <http://time_position>. " +
					"<http://time_position> a <%s>; <%s> 123; <%s> <http://dbpedia.org/resource/Unix_time>.}",
				entity1, namespace + "hasTime", namespace + "Instant", namespace + "inTimePosition",
				namespace + "TimePosition", namespace + "numericPosition", namespace + "hasTRS")
		);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(
				e.getMessage().contains("Entities belonging to a derivation should not have timestamps attached"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 4. sync derivation depend on async - should throw exception
		devClient.createAsyncDerivation(Arrays.asList(entity1), derivedAgentIRI, inputs, true);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2, Arrays.asList(entity1));
		// triples about agent1 that monitors the async derivation are already added at the start of this test method
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(
				e.getMessage().contains("depends on asynchronous derivation"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 5. pure async derivation form a chain for new information
		// create first asynchronous derivation1
		String upstreamDerivationIRI = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, inputs);
		// triples about agent1 that monitors the async derivation are already added at the start of this test method
		// triples about agent2 that monitors the derivation2 are already added at the start of this test method
		// derivation2 is one derivation downstream compared to the derivation1
		// now we create the second derivation given the upstream derivation
		List<String> inputsOfDownstreamDerivation = Arrays.asList(input1, input2, upstreamDerivationIRI);
		devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2,
				inputsOfDownstreamDerivation);

		// validation should pass as timestamp were automatically added for the pure inputs
		Assert.assertTrue(devClient.validateDerivations());
		// if now we manually remove the timestamp of one pure input, it should fail
		// this is also to check the validateDerivations actually pulled derivaitons
		// from KG and performed the checks (i.e. entered the recursive loop)
		mockClient.executeUpdate("PREFIX time: <http://www.w3.org/2006/time#>" +
			"DELETE WHERE { <" + inputs.get(0) + "> time:hasTime ?timeInstant . ?timeInstant a time:Instant ; time:inTimePosition ?timeUnix ." +
				"?timeUnix a time:TimePosition ; time:numericPosition ?timestamp ; time:hasTRS <http://dbpedia.org/resource/Unix_time> . }"
		);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		// after added timestamps for pure inputs, validateDerivations should pass again
		// for this type of derivation structure
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		Assert.assertTrue(devClient.validateDerivations());

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 6. mixed a-/sync derivations form a DAG
		String d0 = devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, inputs);
		// create the first async derivaiton (new info) depending on sync derivation d0
		String d1 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, Arrays.asList(entity1));
		// triples about agent1 that monitors the async derivation are already added at the start of this test method
		// triples about agent2 that monitors the derivation2 are already added at the start of this test method
		// derivation2 is one derivation downstream compared to the derivation1
		// now we create the second derivation given the upstream derivation d0
		String d2 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, Arrays.asList(entity1));

		// and third derivation given the two upstream derivation
		String d3 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI3, Arrays.asList(d1, d2));

		// validation should pass as timestamp were automatically added for the pure inputs
		Assert.assertTrue(devClient.validateDerivations());

		// if now we manually remove the timestamp of one pure input, it should fail
		// this is also to check the validateDerivations actually pulled derivaitons
		// from KG and performed the checks (i.e. entered the recursive loop)
		mockClient.executeUpdate("PREFIX time: <http://www.w3.org/2006/time#>" +
			"DELETE WHERE { <" + inputs.get(0) + "> time:hasTime ?timeInstant . ?timeInstant a time:Instant ; time:inTimePosition ?timeUnix ." +
				"?timeUnix a time:TimePosition ; time:numericPosition ?timestamp ; time:hasTRS <http://dbpedia.org/resource/Unix_time> . }"
		);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		// after added timestamps for pure inputs, validateDerivations should pass again
		// for this type of derivation structure
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		Assert.assertTrue(devClient.validateDerivations());

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
	}

	@Test
	public void testBulkCreateDerivationsDetectCircularDependency() {
		// this test focuses on those situations where DerivationSparql::bulkCreateDerivations works fine
		// but DerivationClient::bulkCreateDerivations should fail thanks to DerivationClient::validateDerivations
		// this will happen if there's something potentiall create a circular dependencies in the list of derivation
		// markup provided as arguments to the DerivationSparql::bulkCreateDerivations
		// but won't be identified with the current design, a better design might be provided in the future to identify
		// any circular dependency at creation, e.g. create a DAG in local memory for all the provided markup and validate

		// initialise rdf:type of all instances, so that derivations can be cached
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);
		// also need to initialise ontoagent instance for agents, so that derivations can be cached
		devClient.createOntoAgentInstance(derivedAgentIRI, derivedAgentURL, Arrays.asList("http://i_1"), Arrays.asList("http://o_1"));
		devClient.createOntoAgentInstance(derivedAgentIRI2, derivedAgentURL2, Arrays.asList("http://i_2"), Arrays.asList("http://o_2"));
		devClient.createOntoAgentInstance(derivedAgentIRI3, derivedAgentURL3, Arrays.asList("http://i_3"), Arrays.asList("http://o_3"));

		// below test cases are same to [2. intentionally create a circular dependency]
		// in DerivedQuantityClientTest::testValidateDerived
		// case i: e1 --> i1, i2. i1 --> e1.
		// if we use the DerivationSparql::bulkCreateDerivations to bulk create
		// it will be successful, but will fail at DerivationClient::bulkCreateDerivations
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(
						Arrays.asList(Arrays.asList(entity1), Arrays.asList(input1)),
						Arrays.asList(derivedAgentIRI, derivedAgentIRI2),
						Arrays.asList(Arrays.asList(input1, input2), Arrays.asList(entity1))));
		Assert.assertTrue(e.getMessage().contains("Circular dependency likely occurred"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// case ii: e2 --> e1. e1 --> i1, i2. i1 --> e1.
		// if we use the DerivationSparql::bulkCreateDerivations to bulk create
		// it will be successful, but will fail at DerivationClient::bulkCreateDerivations
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(
						Arrays.asList(Arrays.asList(entity1), Arrays.asList(entity2), Arrays.asList(input1)),
						Arrays.asList(derivedAgentIRI, derivedAgentIRI2, derivedAgentIRI3),
						Arrays.asList(Arrays.asList(input1, input2), Arrays.asList(entity1), Arrays.asList(entity1))));
		Assert.assertTrue(e.getMessage().contains("Edge would induce a cycle"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
	}

	@Test
	public void testDropDerivations() {
		// this method doesn't delete triples about OntoAgent instances
		// first add the ontoagent instance
		devClient.createOntoAgentInstance(
				derivedAgentIRI, derivedAgentURL,
				Arrays.asList(input1RdfType, input2RdfType),
				Arrays.asList(entity1RdfType, entity2RdfType));
		OntModel testKG = mockClient.getKnowledgeBase();

		// case 1: standard derivation
		String derivation = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		// triples about agentIRI should not be deleted
		Assert.assertNotNull(testKG.getIndividual(derivedAgentIRI));

		// case 2: with time series
		derivation = devClient.createDerivationWithTimeSeries(entities, derivedAgentIRI, inputs);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		// triples about agentIRI should not be deleted
		Assert.assertNotNull(testKG.getIndividual(derivedAgentIRI));

		// case 3: async derivation
		derivation = devClient.createAsyncDerivation(entities, derivedAgentIRI,
				inputs, true);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		// triples about agentIRI should not be deleted
		Assert.assertNotNull(testKG.getIndividual(derivedAgentIRI));

		// case 3: all three types present
		derivation = devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, Arrays.asList(input1));
		String derivation2 = devClient.createDerivationWithTimeSeries(Arrays.asList(entity2), derivedAgentIRI,
				Arrays.asList(input2));
		String derivation3 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI,
				Arrays.asList(entity1, entity2));
		Assert.assertNotNull(testKG.getIndividual(derivation));
		Assert.assertNotNull(testKG.getIndividual(derivation2));
		Assert.assertNotNull(testKG.getIndividual(derivation3));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		Assert.assertNull(testKG.getIndividual(derivation2));
		Assert.assertNull(testKG.getIndividual(derivation3));
		// triples about agentIRI should not be deleted
		Assert.assertNotNull(testKG.getIndividual(derivedAgentIRI));
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

	@Test
	public void testDropTimestampsOf() {
		OntModel testKG = mockClient.getKnowledgeBase();

		// add two timestamps and confirm they are there
		devClient.addTimeInstance(input1);
		devClient.addTimeInstance(input2);
		Assert.assertNotNull(testKG.getIndividual(input1));
		Assert.assertNotNull(testKG.getIndividual(input2));

		// drop one timestamp and confirm it is gone, but the other is still there
		devClient.dropTimestampsOf(Arrays.asList(input1));
		Assert.assertNull(testKG.getIndividual(input1));
		Assert.assertNotNull(testKG.getIndividual(input2));

		// drop a non-existent timestamp and nothing should happen
		Assert.assertNull(testKG.getIndividual(entity1));
		devClient.dropTimestampsOf(Arrays.asList(entity1));
		Assert.assertNull(testKG.getIndividual(entity1));

		// drop a list of timestamps and confirm they are gone
		devClient.dropTimestampsOf(Arrays.asList(input1, input2, entity1));
		Assert.assertNull(testKG.getIndividual(input1));
		Assert.assertNull(testKG.getIndividual(input2));
		Assert.assertNull(testKG.getIndividual(entity1));
	}

	@Test
	public void testRetrieveAgentInputIRIs() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// add triples about agent
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput),
				ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input1));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input2));
		// create async derivation
		String derivationIRI = devClient.createAsyncDerivation(entities, derivedAgentIRI, inputs, false);
		String statusIRI = devClient.sparqlClient.markAsRequested(derivationIRI);
		// check if input IRIs are retrieved correctly
		JSONObject agentInputs = devClient.retrieveAgentInputIRIs(derivationIRI, derivedAgentIRI);
		Assert.assertTrue(agentInputs.has(DerivationClient.AGENT_INPUT_KEY));
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				agentInputs.getJSONObject(DerivationClient.AGENT_INPUT_KEY).getJSONArray(input1).toList().stream()
						.map(i -> (String) i).collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2), agentInputs.getJSONObject(DerivationClient.AGENT_INPUT_KEY)
				.getJSONArray(input2).toList().stream().map(i -> (String) i).collect(Collectors.toList())));
	}

	@Test
	public void testGroupSyncDerivationsToUpdate() {
		List<String> normalD = Arrays.asList("http://d0", "http://d1");
		List<String> dTS = Arrays.asList("http://d2", "http://d3");
		List<String> dSync = Stream.concat(normalD.stream(), dTS.stream()).collect(Collectors.toList());
		List<String> dAsyn = Arrays.asList("http://d4");

		Map<String, List<String>> derivationsToUpdate = new HashMap<>();
		List<String> syncDerivations = devClient.groupSyncDerivationsToUpdate(derivationsToUpdate);
		// syncDerivations is empty as nothing are passed
		Assert.assertTrue(syncDerivations.isEmpty());

		// put only normal Derivation
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATION, normalD);
		syncDerivations = devClient.groupSyncDerivationsToUpdate(derivationsToUpdate);
		Assert.assertTrue(equalLists(normalD, syncDerivations));

		// put only DerivationWithTimeSeries
		derivationsToUpdate = new HashMap<>();
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES, dTS);
		syncDerivations = devClient.groupSyncDerivationsToUpdate(derivationsToUpdate);
		Assert.assertTrue(equalLists(dTS, syncDerivations));

		// put both Derivation and DerivationWithTimeSeries
		derivationsToUpdate = new HashMap<>();
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATION, normalD);
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES, dTS);
		syncDerivations = devClient.groupSyncDerivationsToUpdate(derivationsToUpdate);
		Assert.assertTrue(equalLists(dSync, syncDerivations));

		// put only DerivationAsyn, nothing should be added
		derivationsToUpdate = new HashMap<>();
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATIONASYN, dAsyn);
		syncDerivations = devClient.groupSyncDerivationsToUpdate(derivationsToUpdate);
		Assert.assertTrue(syncDerivations.isEmpty());

		// put DerivationAsyn with normal Derivation and DerivationWithTimeSeries
		derivationsToUpdate = new HashMap<>();
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATION, normalD);
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES, dTS);
		derivationsToUpdate.put(DerivationSparql.ONTODERIVATION_DERIVATIONASYN, dAsyn);
		syncDerivations = devClient.groupSyncDerivationsToUpdate(derivationsToUpdate);
		Assert.assertTrue(equalLists(dSync, syncDerivations));
	}

	@Test
	public void testCleanUpFinishedDerivationUpdate_Case1() {
		// case 1: no downstream derivation, replace existing outputs

		// initialise triples for test
		OntModel testKG = mockClient.getKnowledgeBase();
		List<String> newDerivedIRI = initForCleanUpTests(testKG);

		// create derivation and prepare it for clean up
		String derivation = devClient.createAsyncDerivation(entities, derivedAgentIRI, inputs, true);
		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		inputs.stream().forEach(i -> {
			// no need to addTimeInstance as time instances are automatically added when creating derivation markup
			devClient.sparqlClient.updateTimeStamp(i);
		}); // update timestamp for pure inputs, otherwise
			// updateFinishedAsyncDerivation called by cleanUpFinishedDerivationUpdate will
			// not execute
		devClient.sparqlClient.updateStatusBeforeSetupJob(derivation);
		devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, new ArrayList<>());
		long retrievedInputsAt = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")).getObject()
				.asLiteral().getLong();

		// execute clean up
		devClient.cleanUpFinishedDerivationUpdate(derivation);

		// tests:
		// there should be no status
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		Assert.assertNull(testKG.getIndividual(statusIRI));
		// there should be no retrievedTimestampAt
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")));
		// outputs should be replaced
		for (String iri : newDerivedIRI) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(iri),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
		}
		// old outputs should be deleted
		for (String iri : entities) {
			Assert.assertNull(testKG.getIndividual(iri));
		}
		// timestamp should be the same as the one assigned to retrievedInputsAt
		long newtime = testKG.getIndividual(derivation)
				.getProperty(ResourceFactory.createProperty(p_time + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "numericPosition")).getLong();
		Assert.assertEquals(retrievedInputsAt, newtime);
	}

	@Test
	public void testCleanUpFinishedDerivationUpdate_Case2() {
		// case 2: no downstream derivation, generate new info

		// initialise triples for test
		OntModel testKG = mockClient.getKnowledgeBase();
		List<String> newDerivedIRI = initForCleanUpTests(testKG);

		// create derivation and prepare it for clean up
		String derivation = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, inputs);
		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		inputs.stream().forEach(i -> {
			// no need to addTimeInstance as time instances are automatically added when creating derivation markup
			devClient.sparqlClient.updateTimeStamp(i);
		}); // update timestamp for pure inputs, otherwise
			// updateFinishedAsyncDerivation called by cleanUpFinishedDerivationUpdate will
			// not execute
		devClient.sparqlClient.updateStatusBeforeSetupJob(derivation);
		devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, new ArrayList<>());
		long retrievedInputsAt = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")).getObject()
				.asLiteral().getLong();

		// execute clean up
		devClient.cleanUpFinishedDerivationUpdate(derivation);

		// tests:
		// there should be no status
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		Assert.assertNull(testKG.getIndividual(statusIRI));
		// there should be no retrievedTimestampAt
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")));
		// outputs should be generated
		for (String iri : newDerivedIRI) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(iri),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
		}
		// no old outputs were appended to derivation, so they should still exist
		for (String iri : entities) {
			Assert.assertNotNull(testKG.getIndividual(iri));
		}
		// timestamp should be the same as the one assigned to retrievedInputsAt
		long newtime = testKG.getIndividual(derivation)
				.getProperty(ResourceFactory.createProperty(p_time + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "numericPosition")).getLong();
		Assert.assertEquals(retrievedInputsAt, newtime);
	}

	@Test
	public void testCleanUpFinishedDerivationUpdate_Case3() {
		// case 3: two downstream derivations isDerivedFrom/belongsTo outputs of this
		// derivation

		// initialise triples for test
		OntModel testKG = mockClient.getKnowledgeBase();
		List<String> newDerivedIRI = initForCleanUpTests(testKG);

		// create derivation and prepare it for clean up
		String derivation = devClient.createAsyncDerivation(entities, derivedAgentIRI, inputs, true);
		String derivation2 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, entities);
		String derivation3 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, entities);
		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		inputs.stream().forEach(i -> {
			// no need to addTimeInstance as time instances are automatically added when creating derivation markup
			devClient.sparqlClient.updateTimeStamp(i);
		}); // update timestamp for pure inputs, otherwise
			// updateFinishedAsyncDerivation called by cleanUpFinishedDerivationUpdate will
			// not execute
		devClient.sparqlClient.updateStatusBeforeSetupJob(derivation);
		devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, new ArrayList<>());
		long retrievedInputsAt = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")).getObject()
				.asLiteral().getLong();

		// execute clean up
		devClient.cleanUpFinishedDerivationUpdate(derivation);

		// tests:
		// there should be no status
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		Assert.assertNull(testKG.getIndividual(statusIRI));
		// there should be no retrievedTimestampAt
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")));
		// outputs should be replaced (also connected to the downstream derivations)
		for (String iri : newDerivedIRI) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(iri),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation2),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(iri)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation3),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(iri)));
		}
		// old outputs should be deleted
		for (String iri : entities) {
			Assert.assertNull(testKG.getIndividual(iri));
		}
		// timestamp should be the same as the one assigned to retrievedInputsAt
		long newtime = testKG.getIndividual(derivation)
				.getProperty(ResourceFactory.createProperty(p_time + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "numericPosition")).getLong();
		Assert.assertEquals(retrievedInputsAt, newtime);
	}

	@Test
	public void testCleanUpFinishedDerivationUpdate_Case4() {
		// case 6: directed downstream derivation isDerivedFrom (new info generation)

		// initialise triples for test
		OntModel testKG = mockClient.getKnowledgeBase();
		List<String> newDerivedIRI = initForCleanUpTests(testKG);

		// create derivation and prepare it for clean up
		String derivation = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, inputs);
		String derivation2 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, Arrays.asList(derivation));
		String derivation3 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, Arrays.asList(derivation));
		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		inputs.stream().forEach(i -> {
			// no need to addTimeInstance as time instances are automatically added when creating derivation markup
			devClient.sparqlClient.updateTimeStamp(i);
		}); // update timestamp for pure inputs, otherwise
			// updateFinishedAsyncDerivation called by cleanUpFinishedDerivationUpdate will
			// not execute
		devClient.sparqlClient.updateStatusBeforeSetupJob(derivation);
		devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, new ArrayList<>());
		long retrievedInputsAt = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")).getObject()
				.asLiteral().getLong();

		// execute clean up
		devClient.cleanUpFinishedDerivationUpdate(derivation);

		// tests:
		// there should be no status
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		Assert.assertNull(testKG.getIndividual(statusIRI));
		// there should be no retrievedTimestampAt
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")));
		// outputs should be generated
		for (String iri : newDerivedIRI) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(iri),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation2),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(iri)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation3),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(iri)));
		}
		// no old outputs were appended to derivation, so they should still exist
		for (String iri : entities) {
			Assert.assertNotNull(testKG.getIndividual(iri));
		}
		// direct connection between derivation2/derivation3 and derivation should be
		// deleted
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation2),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
				ResourceFactory.createResource(derivation)));
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation3),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
				ResourceFactory.createResource(derivation)));
		// timestamp should be the same as the one assigned to retrievedInputsAt
		long newtime = testKG.getIndividual(derivation)
				.getProperty(ResourceFactory.createProperty(p_time + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "numericPosition")).getLong();
		Assert.assertEquals(retrievedInputsAt, newtime);
	}

	@Test
	public void testCleanUpFinishedDerivationUpdate_Case5() {
		// case 5: normal and directed downstream derivation co-exist (which should not
		// exist if the derivations were created correctly by developer, but just in
		// case)

		// initialise triples for test
		OntModel testKG = mockClient.getKnowledgeBase();
		List<String> newDerivedIRI = initForCleanUpTests(testKG);

		// create derivation and prepare it for clean up
		String derivation = devClient.createAsyncDerivation(entities, derivedAgentIRI, inputs, true);
		String derivation2 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, entities);
		// NOTE here the developer should create derivation3 with entities (only entity1
		// and entity2) as its inputs, but NOT derivation - this is in fact duplicated
		// declaration
		// here we just create it anyway, and see if the framework is able to handle the
		// situation (correctly update after Finished)
		String derivation3 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2,
				Arrays.asList(derivation, entity1, entity2));
		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		inputs.stream().forEach(i -> {
			// no need to addTimeInstance as time instances are automatically added when creating derivation markup
			devClient.sparqlClient.updateTimeStamp(i);
		}); // update timestamp for pure inputs, otherwise
			// updateFinishedAsyncDerivation called by cleanUpFinishedDerivationUpdate will
			// not execute
		devClient.sparqlClient.updateStatusBeforeSetupJob(derivation);
		devClient.updateStatusAtJobCompletion(derivation, newDerivedIRI, new ArrayList<>());
		long retrievedInputsAt = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")).getObject()
				.asLiteral().getLong();

		// execute clean up
		devClient.cleanUpFinishedDerivationUpdate(derivation);

		// tests:
		// there should be no status
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		Assert.assertNull(testKG.getIndividual(statusIRI));
		// there should be no retrievedTimestampAt
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")));
		// outputs should be replaced
		for (String iri : newDerivedIRI) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(iri),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation2),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(iri)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation3),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(iri)));
		}
		// direct connection between derivation3 and derivation should be deleted
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation3),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
				ResourceFactory.createResource(derivation)));
		// old outputs should be deleted
		for (String iri : entities) {
			Assert.assertNull(testKG.getIndividual(iri));
		}
		// timestamp should be the same as the one assigned to retrievedInputsAt
		long newtime = testKG.getIndividual(derivation)
				.getProperty(ResourceFactory.createProperty(p_time + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(p_time + "numericPosition")).getLong();
		Assert.assertEquals(retrievedInputsAt, newtime);
	}

	////////////////////////////////////////////////////////////
	// Below are utility functions to reduce code-duplication //
	////////////////////////////////////////////////////////////

	/**
	 * This method is requried as now the rdf:type of inputs of derivations are made
	 * compulsory.
	 * 
	 * @param testKG
	 */
	public void initRdfType(OntModel testKG) {
		for (String i : allInstances) {
			testKG.add(ResourceFactory.createResource(i), RDF.type, ResourceFactory.createResource(i + "/rdftype"));
		}
	}

	public List<String> initForCleanUpTests(OntModel testKG) {
		initRdfType(testKG);
		String entity1_new = entity1 + "new";
		String entity2_new = entity2 + "new";
		List<String> newDerivedIRI = Arrays.asList(entity1_new, entity2_new);
		testKG.add(ResourceFactory.createResource(entity1_new), RDF.type,
				ResourceFactory.createResource(entity1 + "/rdftype"));
		testKG.add(ResourceFactory.createResource(entity2_new), RDF.type,
				ResourceFactory.createResource(entity2 + "/rdftype"));
		devClient.createOntoAgentInstance(derivedAgentIRI, derivedAgentURL,
				Arrays.asList(input1 + "/rdftype", input2 + "/rdftype"),
				Arrays.asList(entity1 + "/rdftype", entity2 + "/rdftype"));
		devClient.createOntoAgentInstance(derivedAgentIRI2, derivedAgentURL2,
				Arrays.asList(entity1 + "/rdftype", entity2 + "/rdftype"),
				Arrays.asList(entity3 + "/rdftype", entity4 + "/rdftype"));
		return newDerivedIRI;
	}

	public boolean equalLists(List<String> a, List<String> b) {
		if (a == null && b == null) {
			return true;
		}
		if ((a == null && b != null) || (a != null && b == null) || (a.size() != b.size())) {
			return false;
		}
		Collections.sort(a);
		Collections.sort(b);
		return a.equals(b);
	}
}
