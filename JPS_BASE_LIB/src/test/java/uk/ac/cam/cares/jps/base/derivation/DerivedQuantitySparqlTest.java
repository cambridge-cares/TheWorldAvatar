package uk.ac.cam.cares.jps.base.derivation;

import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
 * createDerivedQuantity, createDerivedQuantityWithTimeSeries, updateTimestamp,
 * addTimeinstance are already tested in DerivedQuantityClientTest
 * 
 * unifiedBulkCreateDerivations is tested by testing other functions that depend
 * on it
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivedQuantitySparqlTest {
	private MockDevStoreClient mockClient;
	private DerivationSparql devClient;
	private final String derivationInstanceBaseURL = "http://derivationsparql/test/";
	private String entity1 = "http://entity1";
	private String entity2 = "http://entity2";
	private String entity3 = "http://entity3";
	private List<String> entities = Arrays.asList(entity1, entity2);

	private String entity4 = "http://entity4";
	private String entity5 = "http://entity5";
	private List<String> entities2 = Arrays.asList(entity4, entity5);

	private String input1 = "http://input1";
	private String input2 = "http://input2";
	private List<String> inputs = Arrays.asList(input1, input2);

	private String input3 = "http://input3";
	private List<String> inputs2 = Arrays.asList(input3);

	private String derivedAgentIRI = "http://derivedagent1";
	private String derivedAgentURL = "http://localhost:8080/derivedagent1";
	private String derivedAgentIRI2 = "http://derivedagent2";
	private String derivedAgentURL2 = "http://localhost:8080/derivedagent2";

	private List<String> agentIRIList = Arrays.asList(derivedAgentIRI, derivedAgentIRI2);
	private List<String> agentURLList = Arrays.asList(derivedAgentURL, derivedAgentURL2);

	// added placeholder instance to be used to form different derivation structures
	private String input0 = "http://input0";
	private String entity6 = "http://entity6";
	private String entity7 = "http://entity7";
	private List<String> inputs0 = Arrays.asList(input0);
	private List<String> inputs1 = Arrays.asList(input1, input2);
	private List<String> inputs3 = Arrays.asList(entity2, entity4, entity5);
	private List<String> entities0 = Arrays.asList(input2, input3);
	private List<String> entities1 = Arrays.asList(entity1, entity2, entity3);
	private List<String> entities3 = Arrays.asList(entity6, entity7);
	private String derivedAgentIRI0 = "http://derivedagent0";
	private String derivedAgentURL0 = "http://localhost:8080/derivedagent0";
	private String derivedAgentIRI1 = "http://derivedagent1";
	private String derivedAgentURL1 = "http://localhost:8080/derivedagent1";
	private String derivedAgentIRI3 = "http://derivedagent3";
	private String derivedAgentURL3 = "http://localhost:8080/derivedagent3";

	// Overall, the four derivations should form a directed acyclic graph (DAG):
	// [i2, i3] <belongsTo> d0. d0 <isDerivedFrom> [i0]; <isDerivedUsing> a0
	// [e1, e2, e3] <belongsTo> d1. d1 <isDerivedFrom> [i1, i2]; <isDerivedUsing> a1
	// [e4, e5] <belongsTo> d2. d2 <isDerivedFrom> [i3]; <isDerivedUsing> a2
	// [e6, e7] <belongsTo> d3. d3 <isDerivedFrom> [e2, e4, e5]; <isDerivedUsing> a3

	// Different parts can be taken to test different structures, for example:
	// (I.1) fragmented case 1 (d1, d2):
	private List<List<String>> inputsListFragmented1 = Arrays.asList(inputs1, inputs2);
	private List<List<String>> entitiesListFragmented1 = Arrays.asList(entities1, entities2);
	private List<String> agentIRIListFragmented1 = Arrays.asList(derivedAgentIRI1, derivedAgentIRI2);
	private List<String> agentURLListFragmented1 = Arrays.asList(derivedAgentURL1, derivedAgentURL2);

	// (I.2) fragmented case 2 (d0, d3):
	private List<List<String>> inputsListFragmented2 = Arrays.asList(inputs0, inputs3);
	private List<List<String>> entitiesListFragmented2 = Arrays.asList(entities0, entities3);
	private List<String> agentIRIListFragmented2 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI3);
	private List<String> agentURLListFragmented2 = Arrays.asList(derivedAgentURL0, derivedAgentURL3);

	// (II.1) chain case 1 (d0, d1):
	private List<List<String>> inputsListChain1 = Arrays.asList(inputs0, inputs1);
	private List<List<String>> entitiesListChain1 = Arrays.asList(entities0, entities1);
	private List<String> agentIRIListChain1 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI1);
	private List<String> agentURLListChain1 = Arrays.asList(derivedAgentURL0, derivedAgentURL1);

	// (II.2) chain case 2 (d0, d2):
	private List<List<String>> inputsListChain2 = Arrays.asList(inputs0, inputs2);
	private List<List<String>> entitiesListChain2 = Arrays.asList(entities0, entities2);
	private List<String> agentIRIListChain2 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI2);
	private List<String> agentURLListChain2 = Arrays.asList(derivedAgentURL0, derivedAgentURL2);

	// (II.3) chain case 3 (d1, d3):
	private List<List<String>> inputsListChain3 = Arrays.asList(inputs1, inputs3);
	private List<List<String>> entitiesListChain3 = Arrays.asList(entities1, entities3);
	private List<String> agentIRIListChain3 = Arrays.asList(derivedAgentIRI1, derivedAgentIRI3);
	private List<String> agentURLListChain3 = Arrays.asList(derivedAgentURL1, derivedAgentURL3);

	// (II.4) chain case 4 (d2, d3):
	private List<List<String>> inputsListChain4 = Arrays.asList(inputs2, inputs3);
	private List<List<String>> entitiesListChain4 = Arrays.asList(entities2, entities3);
	private List<String> agentIRIListChain4 = Arrays.asList(derivedAgentIRI2, derivedAgentIRI3);
	private List<String> agentURLListChain4 = Arrays.asList(derivedAgentURL2, derivedAgentURL3);

	// (II.5) chain case 5 (d0, d1, d3):
	private List<List<String>> inputsListChain5 = Arrays.asList(inputs0, inputs1, inputs3);
	private List<List<String>> entitiesListChain5 = Arrays.asList(entities0, entities1, entities3);
	private List<String> agentIRIListChain5 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI1, derivedAgentIRI3);
	private List<String> agentURLListChain5 = Arrays.asList(derivedAgentURL0, derivedAgentURL1, derivedAgentURL3);

	// (II.6) chain case 6 (d0, d2, d3):
	private List<List<String>> inputsListChain6 = Arrays.asList(inputs0, inputs2, inputs3);
	private List<List<String>> entitiesListChain6 = Arrays.asList(entities0, entities2, entities3);
	private List<String> agentIRIListChain6 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI2, derivedAgentIRI3);
	private List<String> agentURLListChain6 = Arrays.asList(derivedAgentURL0, derivedAgentURL2, derivedAgentURL3);

	// (III) tree (d1, d2, d3):
	private List<List<String>> inputsListTree1 = Arrays.asList(inputs1, inputs2, inputs3);
	private List<List<String>> entitiesListTree1 = Arrays.asList(entities1, entities2, entities3);
	private List<String> agentIRIListTree1 = Arrays.asList(derivedAgentIRI1, derivedAgentIRI2, derivedAgentIRI3);
	private List<String> agentURLListTree1 = Arrays.asList(derivedAgentURL1, derivedAgentURL2, derivedAgentURL3);

	// (IV.1) DAG case 1 (d0, d1, d2):
	private List<List<String>> inputsListDAG1 = Arrays.asList(inputs0, inputs1, inputs2);
	private List<List<String>> entitiesListDAG1 = Arrays.asList(entities0, entities1, entities2);
	private List<String> agentIRIListDAG1 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI1, derivedAgentIRI2);
	private List<String> agentURLListDAG1 = Arrays.asList(derivedAgentURL0, derivedAgentURL1, derivedAgentURL2);

	// (IV.2) DAG case 2 (d0, d1, d2, d3):
	private List<List<String>> inputsListDAG2 = Arrays.asList(inputs0, inputs1, inputs2, inputs3);
	private List<List<String>> entitiesListDAG2 = Arrays.asList(entities0, entities1, entities2, entities3);
	private List<String> agentIRIListDAG2 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI1, derivedAgentIRI2,
			derivedAgentIRI3);
	private List<String> agentURLListDAG2 = Arrays.asList(derivedAgentURL0, derivedAgentURL1, derivedAgentURL2,
			derivedAgentURL3);

	// In total, 11 cases of structure can be tested
	List<List<List<String>>> fragmentedEntititsList = Arrays.asList(entitiesListFragmented1, entitiesListFragmented2);
	List<List<String>> fragmentedAgentIRIList = Arrays.asList(agentIRIListFragmented1, agentIRIListFragmented2);
	List<List<String>> fragmentedAgentURLList = Arrays.asList(agentURLListFragmented1, agentURLListFragmented2);
	List<List<List<String>>> fragmentedInputsList = Arrays.asList(inputsListFragmented1, inputsListFragmented2);

	List<List<List<String>>> chainEntititsList = Arrays.asList(entitiesListChain1, entitiesListChain2,
			entitiesListChain3, entitiesListChain4, entitiesListChain5, entitiesListChain6);
	List<List<String>> chainAgentIRIList = Arrays.asList(agentIRIListChain1, agentIRIListChain2, agentIRIListChain3,
			agentIRIListChain4, agentIRIListChain5, agentIRIListChain6);
	List<List<String>> chainAgentURLList = Arrays.asList(agentURLListChain1, agentURLListChain2, agentURLListChain3,
			agentURLListChain4, agentURLListChain5, agentURLListChain6);
	List<List<List<String>>> chainInputsList = Arrays.asList(inputsListChain1, inputsListChain2, inputsListChain3,
			inputsListChain4, inputsListChain5, inputsListChain6);

	List<List<List<String>>> treeEntititsList = Arrays.asList(entitiesListTree1);
	List<List<String>> treeAgentIRIList = Arrays.asList(agentIRIListTree1);
	List<List<String>> treeAgentURLList = Arrays.asList(agentURLListTree1);
	List<List<List<String>>> treeInputsList = Arrays.asList(inputsListTree1);

	List<List<List<String>>> dagEntititsList = Arrays.asList(entitiesListDAG1, entitiesListDAG2);
	List<List<String>> dagAgentIRIList = Arrays.asList(agentIRIListDAG1, agentIRIListDAG2);
	List<List<String>> dagAgentURLList = Arrays.asList(agentURLListDAG1, agentURLListDAG2);
	List<List<List<String>>> dagInputsList = Arrays.asList(inputsListDAG1, inputsListDAG2);

	List<List<List<String>>> compactEntititsList = Stream
			.of(fragmentedEntititsList, chainEntititsList, treeEntititsList, dagEntititsList)
			.flatMap(Collection::stream).collect(Collectors.toList());
	List<List<String>> compactAgentIRIList = Stream
			.of(fragmentedAgentIRIList, chainAgentIRIList, treeAgentIRIList, dagAgentIRIList)
			.flatMap(Collection::stream).collect(Collectors.toList());
	List<List<String>> compactAgentURLList = Stream
			.of(fragmentedAgentURLList, chainAgentURLList, treeAgentURLList, dagAgentURLList)
			.flatMap(Collection::stream).collect(Collectors.toList());
	List<List<List<String>>> compactInputsList = Stream
			.of(fragmentedInputsList, chainInputsList, treeInputsList, dagInputsList)
			.flatMap(Collection::stream).collect(Collectors.toList());

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
		devClient = new DerivationSparql(mockClient, derivationInstanceBaseURL);
	}

	@After
	public void closeKnowledgeBase() {
		mockClient.closeKnowledgeBase();
	}

	@Test
	public void testHasBelongsTo() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		// empty kg
		Method hasBelongsTo = devClient.getClass().getDeclaredMethod("hasBelongsTo", String.class);
		hasBelongsTo.setAccessible(true);
		Assert.assertFalse((boolean) hasBelongsTo.invoke(devClient, entity1));

		// derived quantity created
		devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		Assert.assertTrue((boolean) hasBelongsTo.invoke(devClient, entity1));
	}

	@Test
	public void testCheckInstanceExists() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
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
		String derivedIRI2 = devClient.createDerivation(Arrays.asList(entity3), derivedAgentIRI, derivedAgentURL,
				entities);
		Assert.assertTrue(devClient.getInputsAndDerived(derivedIRI2).contains(derivedIRI));
	}

	@Test
	public void testGetDerivationsOf() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);

		Map<String, String> derivationsOf = devClient.getDerivationsOf(entities);
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
		// this function is used when a derived quantity is an input to another derived
		// quantity, in this case, entities
		// are inputs to derivedIRI2. The rdftype is used to reconnect instances
		// derivedIRI2 depends on derivedIRI
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, derivedAgentURL, inputs);
		String derivedIRI2 = devClient.createDerivation(Arrays.asList(entity3), derivedAgentIRI, derivedAgentURL,
				entities);

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

		// time stamp of an instance linked to a derived quantity
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
		String entityclass = entity1 + "class";

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
		List<List<String>> inputsList = Arrays.asList(inputs, inputs2);

		Resource derivationType = ResourceFactory.createResource(DerivationSparql.derivednamespace + "Derivation");

		List<String> derivations = devClient.bulkCreateDerivations(entitiesList, agentIRIList, agentURLList,
				inputsList);
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
		List<List<String>> inputsList = Arrays.asList(inputs, inputs2);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationWithTimeSeries");

		List<String> derivations = devClient.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIRIList,
				agentURLList, inputsList);
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
	public void testBulkCreateDerivationsAsync() {
		OntModel testKG = mockClient.getKnowledgeBase();
		List<List<String>> entitiesList = Arrays.asList(entities, entities2);
		List<List<String>> inputsList = Arrays.asList(inputs, inputs2);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");

		List<String> derivations = devClient.bulkCreateDerivationsAsync(entitiesList, agentIRIList,
				agentURLList, inputsList);
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
	public void testBulkCreateMixedDerivations() {
		OntModel testKG = mockClient.getKnowledgeBase();

		List<String> derivationTypeSequence = Arrays.asList(
				DerivationSparql.derivednamespace + "Derivation",
				DerivationSparql.derivednamespace + "DerivationWithTimeSeries",
				DerivationSparql.derivednamespace + "DerivationAsyn");
		List<Resource> derivationTypeResourceList = derivationTypeSequence.stream()
				.map(type -> ResourceFactory.createResource(type))
				.collect(Collectors.toList());

		List<String> derivations = devClient.bulkCreateMixedDerivations(entitiesListChain5, agentIRIListChain5,
				agentURLListChain5, inputsListChain5, derivationTypeSequence);
		for (int i = 0; i < derivations.size(); i++) {
			List<String> entities = entitiesListChain5.get(i);
			List<String> inputs = inputsListChain5.get(i);

			Assert.assertEquals(derivationTypeResourceList.get(i),
					testKG.getIndividual(derivations.get(i)).getRDFType());
			Assert.assertEquals(agentURLListChain5.get(i), devClient.getAgentUrl(derivations.get(i)));

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
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation),
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
		testKG.add(ResourceFactory.createResource(input1), RDF.type, ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(input1RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(input2), RDF.type, ResourceFactory.createResource(input2RdfType));
		testKG.add(ResourceFactory.createResource(input2RdfType), RDFS.subClassOf,
				ResourceFactory.createResource(input2ParentRdfType));

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
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentOperation));
		testKG.add(ResourceFactory.createResource(derivedAgentOperation), ResourceFactory.createProperty(hasInput),
				ResourceFactory.createResource(derivedAgentInputMsgCont));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart1));
		testKG.add(ResourceFactory.createResource(derivedAgentInputMsgCont),
				ResourceFactory.createProperty(hasMandatoryPart), ResourceFactory.createResource(derivedAgentMsgPart2));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart1), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input1RdfType));
		testKG.add(ResourceFactory.createResource(derivedAgentMsgPart2), ResourceFactory.createProperty(hasType),
				ResourceFactory.createResource(input2RdfType));

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
		testKG.add(ResourceFactory.createResource(derivedAgentIRI), ResourceFactory.createProperty(hasOperation),
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
		testKG.add(ResourceFactory.createResource(input1), RDFS.subClassOf,
				ResourceFactory.createResource(input1ParentRdfType));
		testKG.add(ResourceFactory.createResource(input2), RDFS.subClassOf,
				ResourceFactory.createResource(input2ParentRdfType));

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
		String derivationIRI = devClient.createDerivationAsync(Arrays.asList(entity1, entity2, entity3),
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

		// now we retrieve the output instances of the derivation1 that matches the
		// input of derivation2 (OntoAgent I/O of agent2)
		List<String> mappedInstances = devClient.retrieveMatchingInstances(derivationIRI, derivedAgentIRI2);
		// the mappedInstances should be [entity1, entity2]
		mappedInstances.removeAll(Arrays.asList(entity1, entity2));
		Assert.assertTrue(mappedInstances.isEmpty());
	}

	@Test
	public void testGetDerivations() {
		List<List<String>> entitiesList = Arrays.asList(entities, entities2);
		List<List<String>> inputsList = Arrays.asList(inputs, inputs2);

		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesList, agentIRIList, agentURLList,
				inputsList);
		devClient.addTimeInstance(derivationIRIs);

		List<Derivation> derivations = devClient.getDerivations();

		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();

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

	// below test case replaces the test case testGetDerivations()
	// as function getAllDerivationsInKG() replaces function getDerivations()
	@Test
	public void testGetAllDerivationsInKG_Derivation() {
		for (int mode = 0; mode < compactEntititsList.size(); mode++) {
			List<List<String>> _entitiesList = compactEntititsList.get(mode);
			List<String> _agentIRIList = compactAgentIRIList.get(mode);
			List<String> _agentURLList = compactAgentURLList.get(mode);
			List<List<String>> _inputsList = compactInputsList.get(mode);

			List<String> derivationIRIs = devClient.bulkCreateDerivations(_entitiesList, _agentIRIList,
					_agentURLList, _inputsList);
			devClient.addTimeInstance(derivationIRIs);

			OntModel testKG = mockClient.getKnowledgeBase();
			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "Derivation");
			for (String dIRI : derivationIRIs) {
				Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
			}
			assertTestGetAllDerivationsInKG(devClient, derivationIRIs, _entitiesList, _agentURLList, _inputsList);

			devClient.dropAllDerivations();
			devClient.dropAllTimestamps();
			List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
			Assert.assertEquals(0, allDerivations.size());
		}
	}

	// below test case replaces the test case testGetDerivations()
	// as function getAllDerivationsInKG() replaces function getDerivations()
	@Test
	public void testGetAllDerivationsInKG_DerivationAsyn() {
		for (int mode = 0; mode < compactEntititsList.size(); mode++) {
			List<List<String>> _entitiesList = compactEntititsList.get(mode);
			List<String> _agentIRIList = compactAgentIRIList.get(mode);
			List<String> _agentURLList = compactAgentURLList.get(mode);
			List<List<String>> _inputsList = compactInputsList.get(mode);

			List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(_entitiesList, _agentIRIList,
					_agentURLList, _inputsList);
			devClient.addTimeInstance(derivationIRIs);

			OntModel testKG = mockClient.getKnowledgeBase();
			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
			for (String dIRI : derivationIRIs) {
				Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
			}

			assertTestGetAllDerivationsInKG(devClient, derivationIRIs, _entitiesList, _agentURLList, _inputsList);

			devClient.dropAllDerivations();
			devClient.dropAllTimestamps();
			List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
			Assert.assertEquals(0, allDerivations.size());
		}
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_Fragmented_Derivation() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		for (int cas = 0; cas < fragmentedEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = fragmentedEntititsList.get(cas);
			List<String> _agentIRIList = fragmentedAgentIRIList.get(cas);
			List<String> _agentURLList = fragmentedAgentURLList.get(cas);
			List<List<String>> _inputsList = fragmentedInputsList.get(cas);

			List<String> derivationIRIs = devClient.bulkCreateDerivations(_entitiesList, _agentIRIList, _agentURLList,
					_inputsList);
			devClient.addTimeInstance(derivationIRIs);

			OntModel testKG = mockClient.getKnowledgeBase();
			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "Derivation");
			for (String dIRI : derivationIRIs) {
				Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
			}

			assertTestGetRootAndAllUpstreamDerivations_Fragmented(devClient, derivationIRIs,
					_entitiesList, _agentURLList, _inputsList, derivationTypeList);

			devClient.dropAllDerivations();
			devClient.dropAllTimestamps();
			List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
			Assert.assertEquals(0, allDerivations.size());
		}
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_Fragmented_DerivationAsyn() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		for (int cas = 0; cas < fragmentedEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = fragmentedEntititsList.get(cas);
			List<String> _agentIRIList = fragmentedAgentIRIList.get(cas);
			List<String> _agentURLList = fragmentedAgentURLList.get(cas);
			List<List<String>> _inputsList = fragmentedInputsList.get(cas);

			List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(_entitiesList, _agentIRIList,
					_agentURLList, _inputsList);
			devClient.addTimeInstance(derivationIRIs);

			OntModel testKG = mockClient.getKnowledgeBase();
			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
			for (String dIRI : derivationIRIs) {
				Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
			}

			assertTestGetRootAndAllUpstreamDerivations_Fragmented(devClient, derivationIRIs,
					_entitiesList, _agentURLList, _inputsList, derivationTypeList);

			devClient.dropAllDerivations();
			devClient.dropAllTimestamps();
			List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
			Assert.assertEquals(0, allDerivations.size());
		}
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_Chain_Derivation() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		for (int cas = 0; cas < chainEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = chainEntititsList.get(cas);
			List<String> _agentIRIList = chainAgentIRIList.get(cas);
			List<String> _agentURLList = chainAgentURLList.get(cas);
			List<List<String>> _inputsList = chainInputsList.get(cas);

			List<String> derivationIRIs = devClient.bulkCreateDerivations(_entitiesList, _agentIRIList, _agentURLList,
					_inputsList);
			devClient.addTimeInstance(derivationIRIs);

			OntModel testKG = mockClient.getKnowledgeBase();
			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "Derivation");
			for (String dIRI : derivationIRIs) {
				Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
			}

			assertTestGetRootAndAllUpstreamDerivations_Chain(devClient, derivationIRIs,
					_entitiesList, _agentURLList, _inputsList, derivationTypeList);

			devClient.dropAllDerivations();
			devClient.dropAllTimestamps();
			List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
			Assert.assertEquals(0, allDerivations.size());
		}
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_Chain_DerivationAsyn() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		for (int cas = 0; cas < chainEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = chainEntititsList.get(cas);
			List<String> _agentIRIList = chainAgentIRIList.get(cas);
			List<String> _agentURLList = chainAgentURLList.get(cas);
			List<List<String>> _inputsList = chainInputsList.get(cas);

			List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(_entitiesList, _agentIRIList,
					_agentURLList, _inputsList);
			devClient.addTimeInstance(derivationIRIs);

			OntModel testKG = mockClient.getKnowledgeBase();
			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
			for (String dIRI : derivationIRIs) {
				Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
			}

			assertTestGetRootAndAllUpstreamDerivations_Chain(devClient, derivationIRIs,
					_entitiesList, _agentURLList, _inputsList, derivationTypeList);

			devClient.dropAllDerivations();
			devClient.dropAllTimestamps();
			List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
			Assert.assertEquals(0, allDerivations.size());
		}
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_Tree1_Derivation() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		// tree case 1 has structure: d3 --> d1, d3 --> d2, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListTree1, agentIRIListTree1,
				agentURLListTree1, inputsListTree1);
		devClient.addTimeInstance(derivationIRIs);

		OntModel testKG = mockClient.getKnowledgeBase();
		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "Derivation");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		assertTestGetRootAndAllUpstreamDerivations_Tree1(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_Tree1_DerivationAsyn() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		// tree case 1 has structure: d3 --> d1, d3 --> d2, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListTree1, agentIRIListTree1,
				agentURLListTree1, inputsListTree1);
		devClient.addTimeInstance(derivationIRIs);

		OntModel testKG = mockClient.getKnowledgeBase();
		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		assertTestGetRootAndAllUpstreamDerivations_Tree1(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG1_Derivation() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		// DAG case 1 has structure: d1 --> d0, d2 --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListDAG1, agentIRIListDAG1,
				agentURLListDAG1, inputsListDAG1);
		devClient.addTimeInstance(derivationIRIs);

		OntModel testKG = mockClient.getKnowledgeBase();
		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "Derivation");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG1(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG1_DerivationAsyn() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		// DAG case 1 has structure: d1 --> d0, d2 --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListDAG1, agentIRIListDAG1,
				agentURLListDAG1, inputsListDAG1);
		devClient.addTimeInstance(derivationIRIs);

		OntModel testKG = mockClient.getKnowledgeBase();
		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG1(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG2_Derivation() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		// DAG case 2 has structure: d3 --> (d1, d2) --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListDAG2, agentIRIListDAG2,
				agentURLListDAG2, inputsListDAG2);
		devClient.addTimeInstance(derivationIRIs);

		OntModel testKG = mockClient.getKnowledgeBase();
		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "Derivation");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG2(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG2_DerivationAsyn() {
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		// DAG case 2 has structure: d3 --> (d1, d2) --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListDAG2, agentIRIListDAG2,
				agentURLListDAG2, inputsListDAG2);
		devClient.addTimeInstance(derivationIRIs);

		OntModel testKG = mockClient.getKnowledgeBase();
		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG2(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	////////////////////////////////////////////////////////////
	// Below are utility functions to reduce code-duplication //
	////////////////////////////////////////////////////////////
	public void assertTestGetAllDerivationsInKG(DerivationSparql devClient, List<String> derivationIRIs,
			List<List<String>> _entitiesList, List<String> _agentURLList, List<List<String>> _inputsList) {

		List<Derivation> derivations = devClient.getAllDerivationsInKG();

		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();

			List<Entity> inputs = derivation.getInputs();
			for (Entity input : inputs) {
				Assert.assertTrue(_inputsList.get(i).contains(input.getIri()));
			}

			List<Entity> entities = derivation.getEntities();
			for (Entity entity : entities) {
				Assert.assertTrue(_entitiesList.get(i).contains(entity.getIri()));
			}

			Assert.assertEquals(_agentURLList.get(i), derivation.getAgentURL());
		}
	}

	public void assertTestGetRootAndAllUpstreamDerivations_Fragmented(DerivationSparql devClient,
			List<String> derivationIRIs, List<List<String>> _entitiesList, List<String> _agentURLList,
			List<List<String>> _inputsList, List<String> derivationTypeList) {

		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			List<Derivation> derivations = devClient.getRootAndAllTargetUpstreamDerivations(derivationIRI,
					derivationTypeList);

			// for those derivation type that not provided in the (target)derivationTypeList
			// within the argument of this function, make sure NO instances of derivation
			// should be pulled when getRootAndAllTargetUpstreamDerivations, this is
			// specific to this function as it's assumed that none of the derivations
			// outside of
			// the (target)derivationTypeList will be created in the test case
			for (String dType : DerivationSparql.derivationTypes) {
				if (!derivationTypeList.contains(dType)) {
					Assert.assertEquals(0, devClient
							.getRootAndAllTargetUpstreamDerivations(derivationIRI, Arrays.asList(dType)).size());
				}
			}

			Assert.assertEquals(1, derivations.size());
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();

			List<Entity> inputs = derivation.getInputs();
			for (Entity input : inputs) {
				Assert.assertTrue(_inputsList.get(i).contains(input.getIri()));
			}

			List<Entity> entities = derivation.getEntities();
			for (Entity entity : entities) {
				Assert.assertTrue(_entitiesList.get(i).contains(entity.getIri()));
			}

			Assert.assertEquals(_agentURLList.get(i), derivation.getAgentURL());

			assertNoImmediateUpstreamDerivation(derivation);
			assertNoImmediateDownstreamDerivation(derivation);
		}
	}

	public void assertTestGetRootAndAllUpstreamDerivations_Chain(DerivationSparql devClient,
			List<String> derivationIRIs, List<List<String>> _entitiesList, List<String> _agentURLList,
			List<List<String>> _inputsList, List<String> derivationTypeList) {

		for (int devIdx = 0; devIdx < derivationIRIs.size(); devIdx++) {
			List<String> dIRIs = derivationIRIs.subList(0, devIdx + 1);
			List<Derivation> derivations = devClient.getRootAndAllTargetUpstreamDerivations(derivationIRIs.get(devIdx),
					derivationTypeList);

			// for those derivation type that not provided in the (target)derivationTypeList
			// within the argument of this function, make sure NO instances of derivation
			// should be pulled when getRootAndAllTargetUpstreamDerivations, this is
			// specific to this function as it's assumed that none of the derivations
			// outside of
			// the (target)derivationTypeList will be created in the test case
			for (String dType : DerivationSparql.derivationTypes) {
				if (!derivationTypeList.contains(dType)) {
					Assert.assertEquals(0, devClient
							.getRootAndAllTargetUpstreamDerivations(derivationIRIs.get(devIdx),
									Arrays.asList(dType))
							.size());
				}
			}

			for (int i = 0; i < dIRIs.size(); i++) {
				String derivationIRI = dIRIs.get(i);
				Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
						.findFirst().get();

				List<Entity> inputs = derivation.getInputs();
				for (Entity input : inputs) {
					Assert.assertTrue(_inputsList.get(i).contains(input.getIri()));
				}

				List<Entity> entities = derivation.getEntities();
				for (Entity entity : entities) {
					Assert.assertTrue(_entitiesList.get(i).contains(entity.getIri()));
				}

				Assert.assertEquals(_agentURLList.get(i), derivation.getAgentURL());
			}
		}
	}

	public void assertTestGetRootAndAllUpstreamDerivations_Tree1(DerivationSparql devClient,
			List<String> derivationIRIs, List<String> derivationTypeList) {

		for (String derivationIRI : derivationIRIs) {
			List<Derivation> derivations = devClient.getRootAndAllTargetUpstreamDerivations(derivationIRI,
					derivationTypeList);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();

			// for those derivation type that not provided in the (target)derivationTypeList
			// within the argument of this function, make sure NO instances of derivation
			// should be pulled when getRootAndAllTargetUpstreamDerivations, this is
			// specific to this function as it's assumed that none of the derivations
			// outside of
			// the (target)derivationTypeList will be created in the test case
			for (String dType : DerivationSparql.derivationTypes) {
				if (!derivationTypeList.contains(dType)) {
					Assert.assertEquals(0, devClient
							.getRootAndAllTargetUpstreamDerivations(derivationIRI, Arrays.asList(dType)).size());
				}
			}

			if (derivation.getAgentURL().contentEquals(derivedAgentURL1)) {
				// when the retrieved derivation is d1, i.e. the root derivation was d1, there
				// should be only one instance of Derivation (d1), and NO information about
				// upstream nor downstream should be cached
				Assert.assertEquals(1, derivations.size());
				assertD1(derivation);
				// Make sure no information about upstream or downstream are cached
				assertNoImmediateUpstreamDerivation(derivation);
				assertNoImmediateDownstreamDerivation(derivation);
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL2)) {
				// this is the same situation, only d2 but NO information about upstream nor
				// downstream should be cached
				Assert.assertEquals(1, derivations.size());
				assertD2(derivation);
				// Make sure no information about upstream or downstream are cached
				assertNoImmediateUpstreamDerivation(derivation);
				assertNoImmediateDownstreamDerivation(derivation);
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL3)) {
				// when d3 is root, both d1 and d2 should be cached, also they should be
				// connecting to each other
				Assert.assertEquals(3, derivations.size());
				assertD3(derivation);
				// there should be 2 upstream derivations exist for d3
				List<Derivation> upstreamDerivations = collectDistinctImmediateUpstreamDerivations(derivation);
				Assert.assertEquals(2, upstreamDerivations.size());
				for (Derivation upD : upstreamDerivations) {
					if (upD.getAgentURL().contentEquals(derivedAgentURL1)) {
						// when d3 is root, d1 should be cached with NO upstream derivation, but d3 as
						// its downstream
						assertD1(upD);
						assertNoImmediateUpstreamDerivation(upD);
						// the downstream derivation should be d3
						assertHasOnlyImmediateDownstreamD3(upD);
					} else if (upD.getAgentURL().contentEquals(derivedAgentURL2)) {
						// again, d2 should have NO upstream derivation, but d3 as downstream derivation
						assertD2(upD);
						assertNoImmediateUpstreamDerivation(upD);
						// the downstream derivation should be d3
						assertHasOnlyImmediateDownstreamD3(upD);
					} else {
						fail("Unexpected upstream derivation detected for D3 other than D1 and D2: "
								+ upD.getAgentURL());
					}
				}
			} else {
				fail("Unexpected derivation detected: " + derivation.getAgentURL());
			}
		}
	}

	public void assertTestGetRootAndAllUpstreamDerivations_DAG1(DerivationSparql devClient,
			List<String> derivationIRIs, List<String> derivationTypeList) {
		// DAG case 1 has structure: d1 --> d0, d2 --> d0, no connection between d1/d2

		for (String derivationIRI : derivationIRIs) {
			List<Derivation> derivations = devClient.getRootAndAllTargetUpstreamDerivations(derivationIRI,
					derivationTypeList);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();

			// for those derivation type that not provided in the (target)derivationTypeList
			// within the argument of this function, make sure NO instances of derivation
			// should be pulled when getRootAndAllTargetUpstreamDerivations, this is
			// specific to this function as it's assumed that none of the derivations
			// outside of
			// the (target)derivationTypeList will be created in the test case
			for (String dType : DerivationSparql.derivationTypes) {
				if (!derivationTypeList.contains(dType)) {
					Assert.assertEquals(0, devClient
							.getRootAndAllTargetUpstreamDerivations(derivationIRI, Arrays.asList(dType)).size());
				}
			}

			if (derivation.getAgentURL().contentEquals(derivedAgentURL0)) {
				// when the retrieved derivation is d0, i.e. the root derivation was d0, there
				// should be only one instance of Derivation (d0), and NO information about
				// upstream nor downstream should be cached
				Assert.assertEquals(1, derivations.size());
				assertD0(derivation);
				// Make sure no information about upstream or downstream are cached
				assertNoImmediateUpstreamDerivation(derivation);
				assertNoImmediateDownstreamDerivation(derivation);
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL1)) {
				// when d1 is root, both d1 and d0 should be cached and connected to each other,
				// but NO information about d2 should be cached
				Assert.assertEquals(2, derivations.size());
				assertD1(derivation);
				// Make sure no information about downstream are cached
				assertNoImmediateDownstreamDerivation(derivation);
				// Make sure only upstream derivation is d0, and d0 is connected to d1
				assertHasOnlyImmediateUpstreamD0(derivation);
				List<Derivation> _upD = collectDistinctImmediateUpstreamDerivations(derivation);
				Assert.assertEquals(1, _upD.size());
				assertD0(_upD.get(0));
				assertHasOnlyImmediateDownstreamD1(_upD.get(0));
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL2)) {
				// when d2 is root, both d2 and d0 should be cached and connected to each other,
				// but NO information about d1 should be cached
				Assert.assertEquals(2, derivations.size());
				assertD2(derivation);
				// Make sure no information about downstream are cached
				assertNoImmediateDownstreamDerivation(derivation);
				// Make sure only upstream derivation is d0, and d0 is connected to d2
				assertHasOnlyImmediateUpstreamD0(derivation);
				List<Derivation> _upD = collectDistinctImmediateUpstreamDerivations(derivation);
				Assert.assertEquals(1, _upD.size());
				assertD0(_upD.get(0));
				assertHasOnlyImmediateDownstreamD2(_upD.get(0));
			} else {
				fail("Unexpected derivation detected: " + derivation.getAgentURL());
			}
		}
	}

	public void assertTestGetRootAndAllUpstreamDerivations_DAG2(DerivationSparql devClient,
			List<String> derivationIRIs, List<String> derivationTypeList) {
		// DAG case 2 has structure: d3 --> (d1, d2) --> d0, no connection between d1/d2

		for (String derivationIRI : derivationIRIs) {
			List<Derivation> derivations = devClient.getRootAndAllTargetUpstreamDerivations(derivationIRI,
					derivationTypeList);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();

			// for those derivation type that not provided in the (target)derivationTypeList
			// within the argument of this function, make sure NO instances of derivation
			// should be pulled when getRootAndAllTargetUpstreamDerivations, this is
			// specific to this function as it's assumed that none of the derivations
			// outside of
			// the (target)derivationTypeList will be created in the test case
			for (String dType : DerivationSparql.derivationTypes) {
				if (!derivationTypeList.contains(dType)) {
					Assert.assertEquals(0, devClient
							.getRootAndAllTargetUpstreamDerivations(derivationIRI, Arrays.asList(dType)).size());
				}
			}

			if (derivation.getAgentURL().contentEquals(derivedAgentURL0)) {
				// when the retrieved derivation is d0, i.e. the root derivation was d0, there
				// should be only one instance of Derivation (d0), and NO information about
				// upstream nor downstream should be cached
				Assert.assertEquals(1, derivations.size());
				assertD0(derivation);
				// Make sure no information about upstream or downstream are cached
				assertNoImmediateUpstreamDerivation(derivation);
				assertNoImmediateDownstreamDerivation(derivation);
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL1)) {
				// when d1 is root, both d1 and d0 should be cached and connected to each other,
				// but NO information about d2 nor d3 should be cached
				Assert.assertEquals(2, derivations.size());
				assertD1(derivation);
				// Make sure no information about downstream d3 are cached
				assertNoImmediateDownstreamDerivation(derivation);
				// Make sure only upstream derivation is d0, and d0 is connected to d1
				assertHasOnlyImmediateUpstreamD0(derivation);
				List<Derivation> _upD = collectDistinctImmediateUpstreamDerivations(derivation);
				Assert.assertEquals(1, _upD.size());
				assertD0(_upD.get(0));
				assertHasOnlyImmediateDownstreamD1(_upD.get(0));
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL2)) {
				// when d2 is root, both d2 and d0 should be cached and connected to each other,
				// but NO information about d1 nor d3 should be cached
				Assert.assertEquals(2, derivations.size());
				assertD2(derivation);
				// Make sure no information about downstream d3 are cached
				assertNoImmediateDownstreamDerivation(derivation);
				// Make sure only upstream derivation is d0, and d0 is connected to d2
				assertHasOnlyImmediateUpstreamD0(derivation);
				List<Derivation> _upD = collectDistinctImmediateUpstreamDerivations(derivation);
				Assert.assertEquals(1, _upD.size());
				assertD0(_upD.get(0));
				assertHasOnlyImmediateDownstreamD2(_upD.get(0));
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL3)) {
				// when d3 is root, all four derivations should be cached
				Assert.assertEquals(4, derivations.size());
				assertD3(derivation);
				// there should be 2 immediate upstream derivations exist for d3
				List<Derivation> upstreamDerivations = collectDistinctImmediateUpstreamDerivations(derivation);
				Assert.assertEquals(2, upstreamDerivations.size());
				for (Derivation upD : upstreamDerivations) {
					if (upD.getAgentURL().contentEquals(derivedAgentURL1)) {
						// when d3 is root in DAG, d1 should be cached with d0 as its upstream
						// derivation, and d3 as its downstream, d2 should also exist and connected to
						// d0 and d3
						assertD1(upD);
						// the downstream derivation should be d3
						assertHasOnlyImmediateDownstreamD3(upD);
						// Make sure only upstream derivation is d0, and d0 is connected to d1 and d2
						assertHasOnlyImmediateUpstreamD0(upD);
						List<Derivation> _upD = collectDistinctImmediateUpstreamDerivations(upD);
						Assert.assertEquals(1, _upD.size());
						assertD0(_upD.get(0));
						assertHasFullDownstreamD1D2D3(_upD.get(0));
					} else if (upD.getAgentURL().contentEquals(derivedAgentURL2)) {
						// again, d2 should have upstream derivation d0, and d3 as downstream
						// derivation, d1 should also exist and connected to d0 and d3
						assertD2(upD);
						// the downstream derivation should be d3
						assertHasOnlyImmediateDownstreamD3(upD);
						// Make sure only upstream derivation is d0, and d0 is connected to d2
						assertHasOnlyImmediateUpstreamD0(upD);
						List<Derivation> _upD = collectDistinctImmediateUpstreamDerivations(upD);
						Assert.assertEquals(1, _upD.size());
						assertD0(_upD.get(0));
						assertHasFullDownstreamD1D2D3(_upD.get(0));
					} else {
						fail("Unexpected upstream derivation detected for D3 other than D1 and D2: "
								+ upD.getAgentURL());
					}
				}
			} else {
				fail("Unexpected derivation detected: " + derivation.getAgentURL());
			}
		}
	}

	public void assertD0(Derivation d0) {
		Assert.assertEquals(derivedAgentURL0, d0.getAgentURL());
		Assert.assertTrue(equalLists(inputs0, d0.getAgentInputs()));
		Assert.assertTrue(equalLists(entities0, d0.getEntitiesIri()));
	}

	public void assertD1(Derivation d1) {
		Assert.assertEquals(derivedAgentURL1, d1.getAgentURL());
		Assert.assertTrue(equalLists(inputs1, d1.getAgentInputs()));
		Assert.assertTrue(equalLists(entities1, d1.getEntitiesIri()));
	}

	public void assertD2(Derivation d2) {
		Assert.assertEquals(derivedAgentURL2, d2.getAgentURL());
		Assert.assertTrue(equalLists(inputs2, d2.getAgentInputs()));
		Assert.assertTrue(equalLists(entities2, d2.getEntitiesIri()));
	}

	public void assertD3(Derivation d3) {
		Assert.assertEquals(derivedAgentURL3, d3.getAgentURL());
		Assert.assertTrue(equalLists(inputs3, d3.getAgentInputs()));
		Assert.assertTrue(equalLists(entities3, d3.getEntitiesIri()));
	}

	public void assertNoImmediateUpstreamDerivation(Derivation d) {
		boolean noUpstreamDerivation = d.getInputs().stream().allMatch(in -> Objects.isNull(in.getBelongsTo()));
		Assert.assertTrue(noUpstreamDerivation);
	}

	public void assertNoImmediateDownstreamDerivation(Derivation d) {
		boolean noDownstreamDerivation = d.getEntities().stream().allMatch(en -> Objects.isNull((en.getInputOf())));
		Assert.assertTrue(noDownstreamDerivation);
	}

	public void assertHasOnlyImmediateDownstreamD3(Derivation d) {
		List<Derivation> derivations = collectDistinctImmediateDownstreamDerivations(d);
		Assert.assertEquals(1, derivations.size());
		Derivation d3 = derivations.get(0);
		assertD3(d3);
		assertNoImmediateDownstreamDerivation(d3);
	}

	public void assertHasOnlyImmediateUpstreamD0(Derivation d) {
		List<Derivation> derivations = collectDistinctImmediateUpstreamDerivations(d);
		Assert.assertEquals(1, derivations.size());
		Derivation d0 = derivations.get(0);
		assertD0(d0);
		assertNoImmediateUpstreamDerivation(d0);
	}

	public void assertHasOnlyImmediateDownstreamD1(Derivation d) {
		List<Derivation> derivations = collectDistinctImmediateDownstreamDerivations(d);
		Assert.assertEquals(1, derivations.size());
		Derivation d1 = derivations.get(0);
		assertD1(d1);
		assertNoImmediateDownstreamDerivation(d1);
	}

	public void assertHasOnlyImmediateDownstreamD2(Derivation d) {
		List<Derivation> derivations = collectDistinctImmediateDownstreamDerivations(d);
		Assert.assertEquals(1, derivations.size());
		Derivation d2 = derivations.get(0);
		assertD2(d2);
		assertNoImmediateDownstreamDerivation(d2);
	}

	public void assertHasFullUpstreamD0D1D2(Derivation d3) {
		List<Derivation> derivations = collectDistinctImmediateUpstreamDerivations(d3);
		Assert.assertEquals(2, derivations.size());
		for (Derivation d : derivations) {
			if (d.getAgentURL().contentEquals(derivedAgentURL1)) {
				assertD1(d);
				assertHasOnlyImmediateUpstreamD0(d);
			} else if (d.getAgentURL().contentEquals(derivedAgentURL2)) {
				assertD2(d);
				assertHasOnlyImmediateUpstreamD0(d);
			} else {
				fail("Unexpected upstream derivation detected for D3 other than D1 and D2: "
						+ d.getAgentURL());
			}
		}
	}

	public void assertHasFullDownstreamD1D2D3(Derivation d0) {
		List<Derivation> derivations = collectDistinctImmediateDownstreamDerivations(d0);
		Assert.assertEquals(2, derivations.size());
		for (Derivation d : derivations) {
			if (d.getAgentURL().contentEquals(derivedAgentURL1)) {
				// d should have inputs/ouputs/agentURL about d1
				assertD1(d);
				// should be connected to upstream d0
				assertHasOnlyImmediateUpstreamD0(d);
				// should be connected to downstream d3
				assertHasOnlyImmediateDownstreamD3(d);
				// its downstream d3 should be connected to both d1 and d2
				List<Derivation> _d3 = collectDistinctImmediateDownstreamDerivations(d);
				Assert.assertEquals(1, _d3.size());
				Derivation d3 = _d3.get(0);
				assertHasFullUpstreamD0D1D2(d3);
			} else if (d.getAgentURL().contentEquals(derivedAgentURL2)) {
				// d should have inputs/ouputs/agentURL about d2
				assertD2(d);
				// should be connected to upstream d0
				assertHasOnlyImmediateUpstreamD0(d);
				// should be connected to downstream d3
				assertHasOnlyImmediateDownstreamD3(d);
				// its downstream d3 should be connected to both d1 and d2
				List<Derivation> _d3 = collectDistinctImmediateDownstreamDerivations(d);
				Assert.assertEquals(1, _d3.size());
				Derivation d3 = _d3.get(0);
				assertHasFullUpstreamD0D1D2(d3);
			} else {
				fail("Unexpected downstream derivation detected for D0 other than D1 and D2: "
						+ d.getAgentURL());
			}
		}
	}

	// TODO should this function be part of Derivation.java?
	// the return value only reflects the connection between derivations in cached
	// graph, but MIGHT NOT reflect the true situation in the knowledge graph
	public List<Derivation> collectDistinctImmediateUpstreamDerivations(Derivation d) {
		return d.getInputs().stream().filter(in -> Objects.nonNull(in.getBelongsTo())).map(in -> in.getBelongsTo())
				.distinct().collect(Collectors.toList());
	}

	// TODO should this function be part of Derivation.java?
	// the return value only reflects the connection between derivations in cached
	// graph, but MIGHT NOT reflect the true situation in the knowledge graph
	public List<Derivation> collectDistinctImmediateDownstreamDerivations(Derivation d) {
		return d.getEntities().stream().filter(en -> Objects.nonNull(en.getInputOf())).map(en -> en.getInputOf())
				.distinct().collect(Collectors.toList());
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
