package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;

import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
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
 * getDerivationsAndStatusType are provided in
 * DerivedQuantitySparqlTest.java
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivedQuantityClientTest {
	private MockDevStoreClient mockClient;
	private DerivationClient devClient;
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
	private String hasOperation = p_agent + "hasOperation";
	private String hasHttpUrl = p_agent + "hasHttpUrl";
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
	private String derivedAgentOperation2 = "http://derivedagent2/Operation";
	private List<String> allInstances = Arrays.asList(input1, input2, entity1, entity2, entity3, entity4, entity5);

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
		RDFNode operation = testKG.getIndividual(derivedAgentIRI)
				.getProperty(ResourceFactory
						.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation"))
				.getObject();
		RDFNode url = testKG.getIndividual(operation.toString())
				.getProperty(ResourceFactory
						.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl"))
				.getObject();
		Assert.assertEquals(derivedAgentURL, url.toString());

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.createDerivation(entities, derivedAgentIRI3, derivedAgentURL3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testCreateDerivedQuantityWithTimeSeries() {
		String createdDerived = devClient.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI,
				derivedAgentURL, inputs);
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
		RDFNode operation = testKG.getIndividual(derivedAgentIRI)
				.getProperty(ResourceFactory
						.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation"))
				.getObject();
		RDFNode url = testKG.getIndividual(operation.toString())
				.getProperty(ResourceFactory
						.createProperty("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl"))
				.getObject();
		Assert.assertEquals(derivedAgentURL, url.toString());

		// checks for inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(devIndividual,
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}

		// an instance cannot be part of two derived quantities
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient
				.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI3, derivedAgentURL3, inputs));
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
				() -> devClient.createDerivation(entities, derivedAgentIRI3, derivedAgentURL3, inputs));
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
				() -> devClient.createDerivation(entities, derivedAgentIRI3, derivedAgentURL3, inputs));
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
				.createDerivation(Arrays.asList(entity4, entity5), derivedAgentIRI3, derivedAgentURL3, inputs));
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
				.createDerivation(Arrays.asList(entity4, entity5), derivedAgentIRI3, derivedAgentURL3, inputs));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
	}

	@Test
	public void testAddTimeInstance() {
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
	}

	@Test
	public void testUpdateTimestamps() {
		String namespace = "http://www.w3.org/2006/time#";
		String devInstance = devClient.createDerivationWithTimeSeries(Arrays.asList(entity1), derivedAgentIRI,
				derivedAgentURL, inputs);
		OntModel testKG = mockClient.getKnowledgeBase();
		long oldtime = testKG.getIndividual(devInstance)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		devClient.updateTimestamps(Arrays.asList(entity1));
		long newtime = testKG.getIndividual(devInstance)
				.getProperty(ResourceFactory.createProperty(namespace + "hasTime")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "inTimePosition")).getResource()
				.getProperty(ResourceFactory.createProperty(namespace + "numericPosition")).getLong();
		Assert.assertTrue(newtime > oldtime);
	}

	@Test
	public void testValidateDerived() {
		// initialise rdf:type of all instances, so that derivations can be cached
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		// 1. inputs do not have timestamps yet
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI,
				derivedAgentURL, inputs);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2,
				derivedAgentURL2, Arrays.asList(entity1));
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		// test should pass after added timestamp to pure inputs
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}

		Assert.assertTrue(devClient.validateDerivations());

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 2. intentionally create a circular dependency
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2, derivedAgentURL2, Arrays.asList(entity1));
		devClient.createDerivation(inputs, derivedAgentIRI3, derivedAgentURL3, Arrays.asList(entity1));
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("Edge would induce a cycle"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 3. pure inputs part of a derivation
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		devClient.createDerivation(inputs, derivedAgentIRI, derivedAgentURL, inputs);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(
				e.getMessage().contains("Entities belonging to a derivation should not have timestamps attached"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 4. sync derivation depend on async - should throw exception
		devClient.createAsyncDerivation(Arrays.asList(entity1), derivedAgentIRI, inputs, true);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2, derivedAgentURL2, Arrays.asList(entity1));
		// add triples about agent1 that monitors the async derivation
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL));
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(
				e.getMessage().contains("depends on asynchronous derivation"));

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 5. pure async derivation form a chain for new information
		// create first asynchronous derivation1
		String upstreamDerivationIRI = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, inputs);
		// add triples about agent1 that monitors the async derivation
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL));
		// add triples about agent2 that monitors the derivation2 which is one
		// derivation downstream compared to the derivation1
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation2));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation2), ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL2));

		// now we create the second derivation given the upstream derivation
		List<String> inputsOfDownstreamDerivation = Arrays.asList(input1, input2, upstreamDerivationIRI);
		devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2,
				inputsOfDownstreamDerivation);

		// validation should fail as no timestamp were added for the pure inputs
		// this is also to check the validateDerivations actually pulled derivaitons
		// from KG and performed the checks (i.e. entered the recursive loop)
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		// after added timestamps for pure inputs, validateDerivations should also work
		// for this type of derivation structure
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		Assert.assertTrue(devClient.validateDerivations());

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();

		// 6. mixed a-/sync derivations form a DAG
		String d0 = devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		// create the first async derivaiton (new info) depending on sync derivation d0
		devClient.createAsyncDerivationForNewInfo(derivedAgentIRI, Arrays.asList(entity1));
		// add triples about agent1 that monitors the async derivation
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL));
		// add triples about agent2 that monitors the derivation2 which is one
		// derivation downstream compared to the derivation1
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation2));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation2), ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL2));

		// now we create the second derivation given the upstream derivation d0
		devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2, Arrays.asList(entity1));

		// validation should fail as no timestamp were added for the pure inputs
		// this is also to check the validateDerivations actually pulled derivaitons
		// from KG and performed the checks (i.e. entered the recursive loop)
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		// after added timestamps for pure inputs, validateDerivations should also work
		// for this type of derivation structure
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		Assert.assertTrue(devClient.validateDerivations());

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
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
		derivation = devClient.createDerivationWithTimeSeries(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));

		// case 3: async derivation
		derivation = devClient.createAsyncDerivation(entities, derivedAgentIRI,
				inputs, true);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));

		// case 3: all three types present
		derivation = devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL,
				Arrays.asList(input1));
		String derivation2 = devClient.createDerivationWithTimeSeries(Arrays.asList(entity2), derivedAgentIRI,
				derivedAgentURL, Arrays.asList(input2));
		String derivation3 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI2,
				Arrays.asList(entity1, entity2));
		Assert.assertNotNull(testKG.getIndividual(derivation));
		Assert.assertNotNull(testKG.getIndividual(derivation2));
		Assert.assertNotNull(testKG.getIndividual(derivation3));
		devClient.dropAllDerivations();
		Assert.assertNull(testKG.getIndividual(derivation));
		Assert.assertNull(testKG.getIndividual(derivation2));
		Assert.assertNull(testKG.getIndividual(derivation3));
	}

	@Test
	public void testDropDerivationsNotOntoAgent() {
		OntModel testKG = mockClient.getKnowledgeBase();

		// case 1: standard derivation
		String derivation = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivationsNotOntoAgent();
		Assert.assertNull(testKG.getIndividual(derivation));
		// triples about agentIRI should not be deleted
		Assert.assertNotNull(testKG.getIndividual(derivedAgentIRI));

		// case 2: with time series
		derivation = devClient.createDerivationWithTimeSeries(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivationsNotOntoAgent();
		Assert.assertNull(testKG.getIndividual(derivation));
		// triples about agentIRI should not be deleted
		Assert.assertNotNull(testKG.getIndividual(derivedAgentIRI));

		// case 3: async derivation
		derivation = devClient.createAsyncDerivation(entities, derivedAgentIRI,
				inputs, true);
		Assert.assertNotNull(testKG.getIndividual(derivation));
		devClient.dropAllDerivationsNotOntoAgent();
		Assert.assertNull(testKG.getIndividual(derivation));
		// triples about agentIRI should not be deleted
		Assert.assertNotNull(testKG.getIndividual(derivedAgentIRI));

		// case 3: all three types present
		derivation = devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL,
				Arrays.asList(input1));
		String derivation2 = devClient.createDerivationWithTimeSeries(Arrays.asList(entity2), derivedAgentIRI,
				derivedAgentURL, Arrays.asList(input2));
		String derivation3 = devClient.createAsyncDerivationForNewInfo(derivedAgentIRI,
				Arrays.asList(entity1, entity2));
		Assert.assertNotNull(testKG.getIndividual(derivation));
		Assert.assertNotNull(testKG.getIndividual(derivation2));
		Assert.assertNotNull(testKG.getIndividual(derivation3));
		devClient.dropAllDerivationsNotOntoAgent();
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
	public void testRetrieveAgentInputIRIs() {

	}

	@Test
	public void testUpdateStatusAtJobCompletion() {

	}

	@Test
	public void testcheckImmediateUpstreamDerivation() {

	}

	@Test
	public void testGroupSyncDerivationsToUpdate() {

	}

	@Test
	public void testCleanUpFinishedDerivationUpdate() {

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
}
