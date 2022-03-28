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
 * integration tests for updateInstance is provided at
 * TheWorldAvater/Agents/DerivationExample
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
		devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL, inputs);
		devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI2, derivedAgentURL2, Arrays.asList(entity1));

		// inputs do not have timestamps yet
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(e.getMessage().contains("does not have a timestamp"));

		for (String input : inputs) {
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
		for (String input : inputs) {
			devClient.addTimeInstance(input);
		}
		devClient.createDerivation(inputs, derivedAgentIRI, derivedAgentURL, inputs);
		e = Assert.assertThrows(JPSRuntimeException.class, () -> devClient.validateDerivations());
		Assert.assertTrue(
				e.getMessage().contains("Entities belonging to a derivation should not have timestamps attached"));
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
		derivation = devClient.createDerivation(Arrays.asList(entity1), derivedAgentIRI, derivedAgentURL,
				Arrays.asList(input1));
		String derivation2 = devClient.createDerivation(Arrays.asList(entity2), derivedAgentIRI, derivedAgentURL,
				Arrays.asList(input2));
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
