package uk.ac.cam.cares.jps.base.derivation;

import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
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
 * getStatusType is tested while testing testCreateDerivationAsyncForUpdate and
 * testCreateDerivationAsyncForMarkup
 * 
 * mapNewOutputsToDownstream and reconnectAsyncDerivation for async is tested in
 * testCleanUpFinishedDerivationUpdate_Case1~5 in DerivedQuantityClientTest
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 *
 */
public class DerivedQuantitySparqlTest {
	private MockDevStoreClient mockClient;
	private DerivationSparql devClient;
	private final String derivationInstanceBaseURL = "http://derivationsparql/test/";
	private String p_time = "http://www.w3.org/2006/time#";
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
	private final String derivedAgentURL2 = "http://localhost:8080/derivedagent2";

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
	private final String derivedAgentURL0 = "http://localhost:8080/derivedagent0";
	private String derivedAgentIRI1 = "http://derivedagent1";
	private final String derivedAgentURL1 = "http://localhost:8080/derivedagent1";
	private String derivedAgentIRI3 = "http://derivedagent3";
	private final String derivedAgentURL3 = "http://localhost:8080/derivedagent3";

	// two additional derivations to form more complex structure
	private String entity8 = "http://entity8";
	private String entity9 = "http://entity9";
	private List<String> inputs4 = Arrays.asList(input3);
	private List<String> inputs5 = Arrays.asList(entity5, entity8);
	private List<String> entities4 = Arrays.asList(entity8);
	private List<String> entities5 = Arrays.asList(entity9);
	private String derivedAgentIRI4 = "http://derivedagent4";
	private final String derivedAgentURL4 = "http://localhost:8080/derivedagent4";
	private String derivedAgentIRI5 = "http://derivedagent5";
	private final String derivedAgentURL5 = "http://localhost:8080/derivedagent5";

	// Overall, the six derivations should form a directed acyclic graph (DAG):
	// [i2, i3] <belongsTo> d0. d0 <isDerivedFrom> [i0]; <isDerivedUsing> a0
	// [e1, e2, e3] <belongsTo> d1. d1 <isDerivedFrom> [i1, i2]; <isDerivedUsing> a1
	// [e4, e5] <belongsTo> d2. d2 <isDerivedFrom> [i3]; <isDerivedUsing> a2
	// [e6, e7] <belongsTo> d3. d3 <isDerivedFrom> [e2, e4, e5]; <isDerivedUsing> a3
	// [e8] <belongsTo> d4. d4 <isDerivedFrom> [i3]; <isDerivedUsing> a4
	// [e9] <belongsTo> d5. d5 <isDerivedFrom> [e5, e8]; <isDerivedUsing> a5

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

	// (II.7) chain case 7 (d0, d2, d5):
	private List<List<String>> inputsListChain7 = Arrays.asList(inputs0, inputs2, inputs5);
	private List<List<String>> entitiesListChain7 = Arrays.asList(entities0, entities2, entities5);
	private List<String> agentIRIListChain7 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI2, derivedAgentIRI5);
	private List<String> agentURLListChain7 = Arrays.asList(derivedAgentURL0, derivedAgentURL2, derivedAgentURL5);

	// (II.8) chain case 8 (d0, d4, d5):
	private List<List<String>> inputsListChain8 = Arrays.asList(inputs0, inputs4, inputs5);
	private List<List<String>> entitiesListChain8 = Arrays.asList(entities0, entities4, entities5);
	private List<String> agentIRIListChain8 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI4, derivedAgentIRI5);
	private List<String> agentURLListChain8 = Arrays.asList(derivedAgentURL0, derivedAgentURL4, derivedAgentURL5);

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

	// (IV.3) DAG case 3 (d0, d2, d4):
	private List<List<String>> inputsListDAG3 = Arrays.asList(inputs0, inputs2, inputs4);
	private List<List<String>> entitiesListDAG3 = Arrays.asList(entities0, entities2, entities4);
	private List<String> agentIRIListDAG3 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI2,
			derivedAgentIRI4);
	private List<String> agentURLListDAG3 = Arrays.asList(derivedAgentURL0, derivedAgentURL2,
			derivedAgentURL4);

	// (IV.4) DAG case 4 (d0, d2, d4, d5):
	private List<List<String>> inputsListDAG4 = Arrays.asList(inputs0, inputs2, inputs4, inputs5);
	private List<List<String>> entitiesListDAG4 = Arrays.asList(entities0, entities2, entities4, entities5);
	private List<String> agentIRIListDAG4 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI2, derivedAgentIRI4,
			derivedAgentIRI5);
	private List<String> agentURLListDAG4 = Arrays.asList(derivedAgentURL0, derivedAgentURL2, derivedAgentURL4,
			derivedAgentURL5);

	// (IV.5) DAG case 5 (d0, d1, d2, d3, d4, d5):
	private List<List<String>> inputsListDAG5 = Arrays.asList(inputs0, inputs1, inputs2, inputs3, inputs4, inputs5);
	private List<List<String>> entitiesListDAG5 = Arrays.asList(entities0, entities1, entities2, entities3, entities4,
			entities5);
	private List<String> agentIRIListDAG5 = Arrays.asList(derivedAgentIRI0, derivedAgentIRI1, derivedAgentIRI2,
			derivedAgentIRI3, derivedAgentIRI4, derivedAgentIRI5);
	private List<String> agentURLListDAG5 = Arrays.asList(derivedAgentURL0, derivedAgentURL1, derivedAgentURL2,
			derivedAgentURL3, derivedAgentURL4, derivedAgentURL5);

	// In total, 16 cases of structure can be tested
	List<List<List<String>>> fragmentedEntititsList = Arrays.asList(entitiesListFragmented1, entitiesListFragmented2);
	List<List<String>> fragmentedAgentIRIList = Arrays.asList(agentIRIListFragmented1, agentIRIListFragmented2);
	List<List<String>> fragmentedAgentURLList = Arrays.asList(agentURLListFragmented1, agentURLListFragmented2);
	List<List<List<String>>> fragmentedInputsList = Arrays.asList(inputsListFragmented1, inputsListFragmented2);

	List<List<List<String>>> chainEntititsList = Arrays.asList(entitiesListChain1, entitiesListChain2,
			entitiesListChain3, entitiesListChain4, entitiesListChain5, entitiesListChain6, entitiesListChain7,
			entitiesListChain8);
	List<List<String>> chainAgentIRIList = Arrays.asList(agentIRIListChain1, agentIRIListChain2, agentIRIListChain3,
			agentIRIListChain4, agentIRIListChain5, agentIRIListChain6, agentIRIListChain7, agentIRIListChain8);
	List<List<String>> chainAgentURLList = Arrays.asList(agentURLListChain1, agentURLListChain2, agentURLListChain3,
			agentURLListChain4, agentURLListChain5, agentURLListChain6, agentURLListChain7, agentURLListChain8);
	List<List<List<String>>> chainInputsList = Arrays.asList(inputsListChain1, inputsListChain2, inputsListChain3,
			inputsListChain4, inputsListChain5, inputsListChain6, inputsListChain7, inputsListChain8);

	List<List<List<String>>> treeEntititsList = Arrays.asList(entitiesListTree1);
	List<List<String>> treeAgentIRIList = Arrays.asList(agentIRIListTree1);
	List<List<String>> treeAgentURLList = Arrays.asList(agentURLListTree1);
	List<List<List<String>>> treeInputsList = Arrays.asList(inputsListTree1);

	List<List<List<String>>> dagEntititsList = Arrays.asList(entitiesListDAG1, entitiesListDAG2, entitiesListDAG3,
			entitiesListDAG4, entitiesListDAG5);
	List<List<String>> dagAgentIRIList = Arrays.asList(agentIRIListDAG1, agentIRIListDAG2, agentIRIListDAG3,
			agentIRIListDAG4, agentIRIListDAG5);
	List<List<String>> dagAgentURLList = Arrays.asList(agentURLListDAG1, agentURLListDAG2, agentURLListDAG3,
			agentURLListDAG4, agentURLListDAG5);
	List<List<List<String>>> dagInputsList = Arrays.asList(inputsListDAG1, inputsListDAG2, inputsListDAG3,
			inputsListDAG4, inputsListDAG5);

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

	List<String> allInstances = Arrays.asList(input0, input1, input2, input3, entity1, entity2, entity3, entity4,
			entity5, entity6, entity7, entity8, entity9);

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
	private String derivedAgentOperation2 = "http://derivedagent1/Operation2";

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
	public void testCreateOntoAgentInstance() {
		OntModel testKG = mockClient.getKnowledgeBase();
		String ontoAgentServiceIRI = "http://" + UUID.randomUUID().toString();
		String ontoAgentOperationHttpUrl = "http://" + UUID.randomUUID().toString();
		List<String> inputTypes = Arrays.asList("http://" + UUID.randomUUID().toString(), "http://" + UUID.randomUUID().toString());
		List<String> outputTypes = Arrays.asList("http://" + UUID.randomUUID().toString(), "http://" + UUID.randomUUID().toString());
		devClient.createOntoAgentInstance(ontoAgentServiceIRI, ontoAgentOperationHttpUrl, inputTypes, outputTypes);

		// check that triples are added correctly - check httpUrl should be sufficient
		String operationIRI = testKG.getProperty(ResourceFactory.createResource(ontoAgentServiceIRI),
				ResourceFactory.createProperty(hasOperation)).getObject().toString();
		Literal httpUrlAdded = testKG.getProperty(ResourceFactory.createResource(operationIRI),
				ResourceFactory.createProperty(hasHttpUrl)).getObject().asLiteral();
		Assert.assertEquals(ontoAgentOperationHttpUrl, httpUrlAdded.getString());
		Assert.assertEquals(XSD.ANYURI.toString(), httpUrlAdded.getDatatypeURI());

		// retrieve IRI of OntoAgent:Operation instance added for later test
		JSONArray result = mockClient.executeQuery(
				String.format("select ?operation where {<%s> <%s> ?operation.}", ontoAgentServiceIRI, hasOperation));
		Assert.assertEquals(1, result.length());
		String operationIRIAdded = result.getJSONObject(0).getString("operation");

		// create ontoagent instance with the same input arguments again, but nothing should happen
		devClient.createOntoAgentInstance(ontoAgentServiceIRI, ontoAgentOperationHttpUrl, inputTypes, outputTypes);
		// check there should only be one instance of OntoAgent:Operation that got added
		JSONArray result2 = mockClient.executeQuery(
				String.format("select ?operation where {<%s> <%s> ?operation.}", ontoAgentServiceIRI, hasOperation));
		Assert.assertEquals(1, result2.length());
		Assert.assertEquals(operationIRIAdded, result2.getJSONObject(0).getString("operation"));
	}

	@Test
	public void testMarkAsRequested() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		String derivation = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		String statusIRI = devClient.markAsRequested(derivation);
		OntModel testKG = mockClient.getKnowledgeBase();
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus"),
				ResourceFactory.createResource(statusIRI)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
	}

	@Test
	public void testMarkAsError() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// this tests writing exception to triple store
		String derivation = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		// add timestamp to derivations, the timestamp of inputs is automatically added
		devClient.addTimeInstance(derivation);
		// as all inputs' timestamp will be current timestamp, the derivation should be deemed as outdated
		devClient.markAsRequestedIfOutdated(derivation);

		// get an exception by checking if the inputs are allowed to be outputs for other derivations
		JPSRuntimeException exc = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.allowedAsDerivationOutputs(inputs));

		String excComment = devClient.markAsError(derivation, exc);
		Assert.assertEquals(StatusType.ERROR, devClient.getStatusType(derivation));

		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		String errMsg = testKG.getProperty(ResourceFactory.createResource(statusIRI),
				 ResourceFactory.createProperty(RDFS.comment.getURI())).getObject()
				 .asLiteral().getString();
		Assert.assertEquals(excComment, DerivationSparql.escapeSequences(errMsg));

		Assert.assertTrue(errMsg.contains(exc.getClass().toString()));
		Assert.assertTrue(errMsg.contains(exc.getMessage()));
		for (StackTraceElement st : exc.getStackTrace()) {
			Assert.assertTrue(errMsg.contains(st.toString()));
		}
	}

	@Test
	public void testMarkAsErrorEscape() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// this tests writing exception to triple store
		String derivation = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		// add timestamp to derivations, the timestamp of inputs is automatically added
		devClient.addTimeInstance(derivation);
		// as all inputs' timestamp will be current timestamp, the derivation should be deemed as outdated
		devClient.markAsRequestedIfOutdated(derivation);

		// create an error message full of sequence characters
		// System.getProperty("user.dir") should give a file path which normally appears in exception message when running in python
		String sequences = System.getProperty("user.dir") + "\\\t\n\r\b\f\"'";
		JPSRuntimeException sequencesExc = new JPSRuntimeException(sequences);

		String excComment = devClient.markAsError(derivation, sequencesExc);
		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		String errMsg = testKG.getProperty(ResourceFactory.createResource(statusIRI),
				 ResourceFactory.createProperty(RDFS.comment.getURI())).getObject()
				 .asLiteral().getString();
		// the returned value from devClient.markAsError should match the added to triple store
		Assert.assertEquals(excComment, DerivationSparql.escapeSequences(errMsg));
	}

	@Test
	public void testGetDerivationsInErrorStatus() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// this tests writing exception to triple store
		String derivation = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		String derivation2 = devClient.createDerivation(entities2, derivedAgentIRI, inputs);
		// add timestamp to derivations, the timestamp of inputs is automatically added
		devClient.addTimeInstance(derivation);
		devClient.addTimeInstance(derivation2);
		// as all inputs' timestamp will be current timestamp, the derivation should be deemed as outdated
		devClient.markAsRequestedIfOutdated(derivation);
		devClient.markAsRequestedIfOutdated(derivation2);

		// get an exception by checking if the inputs are allowed to be outputs for other derivations
		JPSRuntimeException exc = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.allowedAsDerivationOutputs(inputs));

		String excComment = devClient.markAsError(derivation, exc);
		String excComment2 = devClient.markAsError(derivation2, exc);
		Assert.assertEquals(StatusType.ERROR, devClient.getStatusType(derivation));
		Assert.assertEquals(StatusType.ERROR, devClient.getStatusType(derivation2));

		String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		String errMsg = testKG.getProperty(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDFS.comment.getURI())).getObject()
				.asLiteral().getString();
		String statusIRI2 = testKG.getProperty(ResourceFactory.createResource(derivation2),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		String errMsg2 = testKG.getProperty(ResourceFactory.createResource(statusIRI2),
				ResourceFactory.createProperty(RDFS.comment.getURI())).getObject()
				.asLiteral().getString();
		Assert.assertEquals(errMsg, errMsg2);
		Assert.assertEquals(excComment, errMsg.replace("\n", "\\n"));
		Assert.assertEquals(excComment2, errMsg.replace("\n", "\\n"));
		List<Derivation> derivations = devClient.getDerivationsInErrorStatus(derivedAgentIRI);
		Assert.assertEquals(2, derivations.size());
		Assert.assertEquals(errMsg, derivations.get(0).getErrMsg());
		Assert.assertEquals(errMsg, derivations.get(1).getErrMsg());

		Assert.assertTrue(errMsg.contains(exc.getClass().toString()));
		Assert.assertTrue(errMsg.contains(exc.getMessage()));
		for (StackTraceElement st : exc.getStackTrace()) {
			Assert.assertTrue(errMsg.contains(st.toString()));
		}
	}

	@Test
	public void testMarkAsRequestedIfOutdated() throws InterruptedException {
		String upstreamDerivation = devClient.createDerivation(new ArrayList<>(), derivedAgentIRI, inputs);
		String downstreamDerivation = devClient.createDerivation(new ArrayList<>(), derivedAgentIRI, Arrays.asList(upstreamDerivation));
		// add timestamp to derivations, the timestamp of inputs is automatically added
		devClient.addTimeInstance(Arrays.asList(upstreamDerivation, downstreamDerivation));
		// here we also need to updateTimeStamp to make the upstream derivation up-to-date
		// as the timestamp of inputs is added as current timestamp
		devClient.updateTimeStamp(upstreamDerivation);

		// case 1: as all timestamp will be current timestamp, the upstream derivation should be deemed as
		// up-to-date, thus nothing should happen if execute
		devClient.markAsRequestedIfOutdated(upstreamDerivation);
		OntModel testKG = mockClient.getKnowledgeBase();
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(upstreamDerivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		// for the downstream derivations, the status should be mark as requested as its timestamp
		// is initialised as 0, also this tests the case where no outputs are generated from the upstream
		devClient.markAsRequestedIfOutdated(downstreamDerivation);
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(downstreamDerivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		String downstreamStatusIRI = testKG.getProperty(ResourceFactory.createResource(downstreamDerivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(downstreamStatusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));

		// case 2: if now we make the derivation to be outdated, then the status should
		// be mark as requested, here we sleep for 1 sec to be sure
		TimeUnit.SECONDS.sleep(1);
		for (String input : inputs) {
			devClient.updateTimeStamp(input);
		}
		devClient.markAsRequestedIfOutdated(upstreamDerivation);
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(upstreamDerivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		String statusIRI = testKG.getProperty(ResourceFactory.createResource(upstreamDerivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));

		// case 3: if now we execute to mark up again, then nothing should happen
		devClient.markAsRequestedIfOutdated(upstreamDerivation);
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(upstreamDerivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus"),
				ResourceFactory.createResource(statusIRI)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
	}

	@Test
	public void testUpdateStatusBeforeSetupJob()
			throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException, InterruptedException {
		OntModel testKG = mockClient.getKnowledgeBase();
		// create derivation for markup, no status should be added
		String derivation = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, false);
		Assert.assertFalse(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		// mark derivation as Requested
		String statusIRI = devClient.markAsRequested(derivation);
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus"),
				ResourceFactory.createResource(statusIRI)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
		// update the status, the rdf:type of status should be updated but its IRI
		// should remain the same, the timestamp should also exist and NOT less than
		// "currentTimestamp", the update method itself should also return true
		long currentTimestamp = Instant.now().getEpochSecond();
		Assert.assertTrue(devClient.updateStatusBeforeSetupJob(derivation));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus"),
				ResourceFactory.createResource(statusIRI)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "InProgress")));
		long retrievedInputsAt = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")).getObject()
				.asLiteral().getLong();
		Assert.assertTrue(retrievedInputsAt >= currentTimestamp);
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "uuidLock")));
		String uuid = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "uuidLock")).getObject()
				.asLiteral().getString();
		// now we execute the function again, nothing should happen as data property
		// retrievedInputsAt already exist
		// sleep for 2 sec to make sure the timestamp generated in
		// updateStatusBeforeSetupJob will be different from the existing one
		// the update method call should return false
		TimeUnit.SECONDS.sleep(2);
		Assert.assertFalse(devClient.updateStatusBeforeSetupJob(derivation));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus"),
				ResourceFactory.createResource(statusIRI)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "InProgress")));
		long retrievedInputsAt_new = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "retrievedInputsAt")).getObject()
				.asLiteral().getLong();
		Assert.assertEquals(retrievedInputsAt, retrievedInputsAt_new);
		String uuid_new = testKG.getProperty(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "uuidLock")).getObject()
				.asLiteral().getString();
		Assert.assertEquals(uuid, uuid_new);
	}

	@Test
	public void testAddUuidLockToFinishedStatus() {
		OntModel testKG = mockClient.getKnowledgeBase();
		// (1) should return false for a dummy IRI
		Assert.assertFalse(devClient.addUuidLockToFinishedStatus("http://dummy_derivation"));

		// (2) should return false for a derivation that is up-to-date
		// create derivation for markup, no status should be added
		String derivation = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, false);
		Assert.assertFalse(devClient.addUuidLockToFinishedStatus(derivation));

		// (3) should return false for a derivation with status other than Finished
		// mark derivation as Requested
		String statusIRI = devClient.markAsRequested(derivation);
		Assert.assertFalse(devClient.addUuidLockToFinishedStatus(derivation));

		// (3) should return true for a derivation with status Finished, also no uuidLock yet
		// add a placeholder uuidLock which in actual use case will be generated by calling updateStatusBeforeSetupJob
		// here is just to let the method updateStatusAtJobCompletion able to execute
		testKG.add(ResourceFactory.createResource(derivation),
			ResourceFactory.createProperty(DerivationSparql.derivednamespace + "uuidLock"),
			UUID.randomUUID().toString());
		// update the status, the rdf:type of status should be updated but its IRI
		// should remain the same, the new derived IRIs should also be added
		// also the new triples should be added
		String s = "http://" + UUID.randomUUID().toString();
		String p = "http://" + UUID.randomUUID().toString();
		String o = "http://" + UUID.randomUUID().toString();
		TriplePattern newTriple = Rdf.iri(s).has(Rdf.iri(p), Rdf.iri(o));
		devClient.updateStatusAtJobCompletion(derivation, entities3, Arrays.asList(newTriple));
		Assert.assertTrue(devClient.addUuidLockToFinishedStatus(derivation));

		// (4) should return false for a derivation with status Finished, and uuidLock already
		Assert.assertFalse(devClient.addUuidLockToFinishedStatus(derivation));
	}

	@Test
	public void testUpdateStatusAtJobCompletion()
			throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		OntModel testKG = mockClient.getKnowledgeBase();
		// create derivation for markup, no status should be added
		String derivation = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, false);
		// mark derivation as Requested
		String statusIRI = devClient.markAsRequested(derivation);
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
		// add a placeholder uuidLock which in actual use case will be generated by calling updateStatusBeforeSetupJob
		// here is just to let the method updateStatusAtJobCompletion able to execute
		testKG.add(ResourceFactory.createResource(derivation),
			ResourceFactory.createProperty(DerivationSparql.derivednamespace + "uuidLock"),
			UUID.randomUUID().toString());
		// update the status, the rdf:type of status should be updated but its IRI
		// should remain the same, the new derived IRIs should also be added
		// also the new triples should be added
		String s = "http://" + UUID.randomUUID().toString();
		String p = "http://" + UUID.randomUUID().toString();
		String o = "http://" + UUID.randomUUID().toString();
		TriplePattern newTriple = Rdf.iri(s).has(Rdf.iri(p), Rdf.iri(o));
		devClient.updateStatusAtJobCompletion(derivation, entities3, Arrays.asList(newTriple));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus"),
				ResourceFactory.createResource(statusIRI)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(RDF.type.getURI()),
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Finished")));
		for (String e : entities3) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasNewDerivedIRI"),
					ResourceFactory.createResource(e)));
		}
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(s),
				ResourceFactory.createProperty(p), ResourceFactory.createResource(o)));
	}

	@Test
	public void testGetNewDerivedIRI()
			throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		OntModel testKG = mockClient.getKnowledgeBase();
		// create derivation with status marked as "Requested"
		String derivation = devClient.createDerivationAsync(entities, derivedAgentIRI, inputs, true);
		// add a placeholder uuidLock which in actual use case will be generated by calling updateStatusBeforeSetupJob
		// here is just to let the method updateStatusAtJobCompletion able to execute
		testKG.add(ResourceFactory.createResource(derivation),
			ResourceFactory.createProperty(DerivationSparql.derivednamespace + "uuidLock"),
			UUID.randomUUID().toString());
		// update the status, the new derived IRIs should be added
		devClient.updateStatusAtJobCompletion(derivation, entities3, new ArrayList<>());
		List<String> newDerivedIRI = devClient.getNewDerivedIRI(derivation);
		Assert.assertTrue(equalLists(newDerivedIRI, entities3));
	}

	@Test
	public void testIsDerivedAsynchronous()
			throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		// create three derivations
		String derivation1 = devClient.createDerivationWithTimeSeries(entities1, derivedAgentIRI1, inputs1);
		String derivation2 = devClient.createDerivation(entities2, derivedAgentIRI2, inputs2);
		String derivation3 = devClient.createDerivationAsync(entities3, derivedAgentIRI3, inputs3, false);
		// assert derivation type
		Assert.assertTrue(!devClient.isDerivedAsynchronous(derivation1));
		Assert.assertTrue(!devClient.isDerivedAsynchronous(derivation2));
		Assert.assertTrue(devClient.isDerivedAsynchronous(derivation3));
	}

	@Test
	public void testGetDerivations_GivenAgentIRI() {
		String derivation1 = devClient.createDerivationWithTimeSeries(entities1, derivedAgentIRI1, inputs1);
		String derivation2 = devClient.createDerivation(entities2, derivedAgentIRI1, inputs2);
		String derivation3 = devClient.createDerivationAsync(entities3, derivedAgentIRI1, inputs3, false);
		List<String> derivations = devClient.getDerivations(derivedAgentIRI1);
		Assert.assertTrue(equalLists(Arrays.asList(derivation1, derivation2, derivation3), derivations));
	}

	@Test
	public void testGetAgentUrl() {
		// create placeholder ontoagent instance to get derivedAgentIRI and derivedAgentURL connected
		devClient.createOntoAgentInstance(derivedAgentIRI, derivedAgentURL, Arrays.asList("http://i"), Arrays.asList("http://o"));
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		Assert.assertEquals(derivedAgentURL, devClient.getAgentUrl(derivedIRI));
	}

	@Test
	public void testGetAgentUrlGivenAgentIRI() {
		// create placeholder ontoagent instance to get derivedAgentIRI and derivedAgentURL connected
		devClient.createOntoAgentInstance(derivedAgentIRI, derivedAgentURL, Arrays.asList("http://i"), Arrays.asList("http://o"));
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		Assert.assertEquals(derivedAgentURL, devClient.getAgentUrlGivenAgentIRI(derivedAgentIRI));
	}

	@Test
	public void testGetInputs() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		List<String> queriedInputs = devClient.getInputs(derivedIRI);

		for (String queriedInput : queriedInputs) {
			Assert.assertTrue(inputs.contains(queriedInput));
		}
	}

	@Test
	public void testGetInputsAndDerived() {
		// when an input is not a derived quantity
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		Assert.assertTrue(devClient.getInputsAndDerived(derivedIRI).containsAll(inputs));

		// when an input is a derived quantity
		String derivedIRI2 = devClient.createDerivation(Arrays.asList(entity3), derivedAgentIRI, entities);
		Assert.assertTrue(devClient.getInputsAndDerived(derivedIRI2).contains(derivedIRI));
	}

	@Test
	public void testGetDerivationsOf() {
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, inputs);

		Map<String, String> derivationsOf = devClient.getDerivationsOf(entities);
		for (String entity : entities) {
			Assert.assertEquals(derivedIRI, derivationsOf.get(entity));
		}
	}

	@Test
	public void testGetTimestamp() {
		// no time stamp yet
		Assert.assertThrows(JPSRuntimeException.class, () -> devClient.getTimestamp(input1));

		// timestamp attached directly to input
		devClient.addTimeInstance(input1);
		devClient.getTimestamp(input1);

		// time stamp of an instance linked to a derived quantity
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		devClient.addTimeInstance(derivedIRI);

		for (String entity : entities) {
			Assert.assertEquals(devClient.getTimestamp(derivedIRI), devClient.getTimestamp(entity));
		}
	}

	@Test
	public void testUpdateTimestamp() {
		// simply checks new time stamp is more recent
		String derivedIRI = devClient.createDerivation(entities, derivedAgentIRI, inputs);
		devClient.addTimeInstance(derivedIRI);
		// the derived instance is initialised with timestamp = 0
		long oldtime = devClient.getTimestamp(derivedIRI);
		devClient.updateTimeStamp(derivedIRI);
		long newtime = devClient.getTimestamp(derivedIRI);
		Assert.assertTrue(newtime > oldtime);
	}

	@Test
	public void testUpdateTimestampDeleteStatus() throws InterruptedException {
		OntModel testKG = mockClient.getKnowledgeBase();
		long timestamp = Instant.now().getEpochSecond();
		// create DerivationWithTimeSeries
		String derivation = devClient.createDerivationWithTimeSeries(entities, derivedAgentIRI, inputs);
		devClient.addTimeInstance(derivation); // timestamp initialised as 0

		// case 1: no status are added, only update timestamp
		Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
		// update timestamp and delete status
		devClient.updateTimestampDeleteStatus(derivation, timestamp);

		// timestamp should be updated
		long addedTimestamp = devClient.getTimestamp(derivation);
		Assert.assertEquals(timestamp, addedTimestamp);

		// case 2: derivation is marked with status
		String statusIRI = devClient.markAsRequested(derivation);
		Assert.assertNotNull(testKG.getIndividual(statusIRI));
		// add triples about new derived IRI
		testKG.add(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasNewDerivedIRI"),
				ResourceFactory.createResource(entity1));
		testKG.add(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasNewDerivedIRI"),
				ResourceFactory.createResource(entity2));

		// update timestamp and delete status
		TimeUnit.SECONDS.sleep(2);
		long newTS = Instant.now().getEpochSecond();
		devClient.updateTimestampDeleteStatus(derivation, newTS);

		// status and the connection with new derived IRI should be deleted
		Assert.assertNull(testKG.getIndividual(statusIRI));
		// timestamp should be updated
		addedTimestamp = devClient.getTimestamp(derivation);
		Assert.assertEquals(newTS, addedTimestamp);
	}

	@Test
	public void testReconnectSyncDerivation() {
		OntModel testKG = mockClient.getKnowledgeBase();
		List<String> oldInstances = Arrays.asList("http://a", "http://b", "http://c");
		List<TriplePattern> newTriples1 = new ArrayList<>();
		newTriples1.add(Rdf.iri("http://a/new").isA(Rdf.iri("http://a/rdftype")));
		newTriples1.add(Rdf.iri("http://b/new").isA(Rdf.iri("http://b/rdftype")));
		newTriples1.add(Rdf.iri("http://c/new").isA(Rdf.iri("http://c/rdftype")));
		Map<String, List<String>> newInstanceMap1 = new HashMap<>();
		newInstanceMap1.put("http://a/new", Arrays.asList("http://d1", "http://d2"));
		newInstanceMap1.put("http://b/new", Arrays.asList("http://d3"));
		newInstanceMap1.put("http://c/new", new ArrayList<String>());
		Map<String, List<String>> newInstanceMap2 = new HashMap<>();
		newInstanceMap2.put("http://a/new2", Arrays.asList("http://d1", "http://d2"));
		newInstanceMap2.put("http://b/new2", Arrays.asList("http://d3"));
		newInstanceMap2.put("http://c/new2", new ArrayList<String>());
		List<TriplePattern> newTriples2 = new ArrayList<>();
		newTriples2.add(Rdf.iri("http://a/new2").isA(Rdf.iri("http://a/rdftype")));
		newTriples2.add(Rdf.iri("http://b/new2").isA(Rdf.iri("http://b/rdftype")));
		newTriples2.add(Rdf.iri("http://c/new2").isA(Rdf.iri("http://c/rdftype")));
		String derivation = devClient.createDerivation(oldInstances, derivedAgentIRI, inputs);
		devClient.addTimeInstance(derivation); // timestamp initialised as 0
		// timestamp for all inputs should already be added automatically when createDerivation

		// test if derivation was created correctly
		// agent
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				ResourceFactory.createResource(derivedAgentIRI)));
		// outputs
		for (String instance : oldInstances) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(instance),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
		}
		// inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}
		// new instance set 1 should NOT be created
		newInstanceMap1.keySet().stream().forEach(instance -> {
			Assert.assertTrue(Objects.isNull(testKG.getIndividual(instance)));
		});
		// new instance set 2 should NOT be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assert.assertTrue(Objects.isNull(testKG.getIndividual(instance)));
		});

		// case 1: derivation is outdated, now delete and add new instances should work
		long retrievedInputsAt = Instant.now().getEpochSecond();
		devClient.reconnectSyncDerivation(derivation, newInstanceMap1, newTriples1, retrievedInputsAt);
		// all old outputs should be deleted
		for (String instance : oldInstances) {
			Assert.assertTrue(Objects.isNull(testKG.getIndividual(instance)));
		}
		// new outputs should be connected to this derivation, and its downstreams
		newInstanceMap1.forEach((instance, dds) -> {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(instance),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(instance), RDF.type));
			if (!dds.isEmpty()) {
				dds.stream().forEach(dd -> {
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(dd),
							ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
							ResourceFactory.createResource(instance)));
				});
			}
		});
		// timestamp should be updated
		Assert.assertEquals(retrievedInputsAt, devClient.getTimestamp(derivation));
		// new instance set 2 should not be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assert.assertTrue(Objects.isNull(testKG.getIndividual(instance)));
		});

		// case 2: as now the timestamp of this derivation is up-to-date, nothing should
		// happen when reconnectNewDerivedIRIs again with a new sets of instances
		devClient.reconnectSyncDerivation(derivation, newInstanceMap2, newTriples2, retrievedInputsAt);
		// repeat all checks after case 1
		// all old outputs should be deleted
		for (String instance : oldInstances) {
			Assert.assertTrue(Objects.isNull(testKG.getIndividual(instance)));
		}
		// new outputs should be connected to this derivation, and its downstreams
		newInstanceMap1.forEach((instance, dds) -> {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(instance),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(instance), RDF.type));
			if (!dds.isEmpty()) {
				dds.stream().forEach(dd -> {
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(dd),
							ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
							ResourceFactory.createResource(instance)));
				});
			}
		});
		// timestamp should be updated
		Assert.assertEquals(retrievedInputsAt, devClient.getTimestamp(derivation));
		// new instance set 2 should not be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assert.assertTrue(Objects.isNull(testKG.getIndividual(instance)));
		});
	}

	@Test
	public void testCreateDerivationIRI() {
		String derivationIRI = devClient.createDerivationIRI(DerivationSparql.ONTODERIVATION_DERIVATION);
		Assert.assertTrue(derivationIRI.startsWith(derivationInstanceBaseURL + DerivationSparql.DERIVATION + "_"));

		derivationIRI = devClient.createDerivationIRI(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES);
		Assert.assertTrue(derivationIRI.startsWith(derivationInstanceBaseURL + DerivationSparql.DERIVATIONWITHTIMESERIES + "_"));

		derivationIRI = devClient.createDerivationIRI(DerivationSparql.ONTODERIVATION_DERIVATIONASYN);
		Assert.assertTrue(derivationIRI.startsWith(derivationInstanceBaseURL + DerivationSparql.DERIVATIONASYN + "_"));
	}

	@Test
	public void testWriteSyncDerivationNewInfo() {
		OntModel testKG = mockClient.getKnowledgeBase();

		// output triples of the derivation
		List<TriplePattern> newTriples = new ArrayList<>();
		String a = "http://a/instance" + UUID.randomUUID().toString();
		String b = "http://b/instance" + UUID.randomUUID().toString();
		String c = "http://c/instance" + UUID.randomUUID().toString();
		String a_p = "http://a/property" + UUID.randomUUID().toString();
		String b_p = "http://b/property" + UUID.randomUUID().toString();
		String c_p = "http://c/property" + UUID.randomUUID().toString();
		String a_v = "http://a/value" + UUID.randomUUID().toString();
		String b_v = "http://b/value" + UUID.randomUUID().toString();
		String c_v = "http://c/value" + UUID.randomUUID().toString();
		newTriples.add(Rdf.iri(a).has(Rdf.iri(a_p), Rdf.iri(a_v)));
		newTriples.add(Rdf.iri(b).has(Rdf.iri(b_p), Rdf.iri(b_v)));
		newTriples.add(Rdf.iri(c).has(Rdf.iri(c_p), Rdf.iri(c_v)));

		// create a new derivation IRI
		String derivationType = DerivationSparql.ONTODERIVATION_DERIVATION;
		String derivation = devClient.createDerivationIRI(derivationType);

		// timestamp
		long retrievedInputsAt = Instant.now().getEpochSecond();

		devClient.writeSyncDerivationNewInfo(newTriples, entities, derivedAgentIRI, inputs, derivation,
				derivationType, retrievedInputsAt);

		// test if derivation was created correctly
		// derivation
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation), RDF.type,
				ResourceFactory.createResource(derivationType)));
		// agent
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedUsing"),
				ResourceFactory.createResource(derivedAgentIRI)));
		// outputs
		for (String instance : entities) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(instance),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
					ResourceFactory.createResource(derivation)));
		}
		// inputs
		for (String input : inputs) {
			Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivation),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
					ResourceFactory.createResource(input)));
		}
		// timestamp
		Assert.assertEquals(retrievedInputsAt, devClient.getTimestamp(derivation));
		// outputs triples
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(a),
				ResourceFactory.createProperty(a_p), ResourceFactory.createResource(a_v)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(b),
				ResourceFactory.createProperty(b_p), ResourceFactory.createResource(b_v)));
		Assert.assertTrue(testKG.contains(ResourceFactory.createResource(c),
				ResourceFactory.createProperty(c_p), ResourceFactory.createResource(c_v)));
	}

	@Test
	public void testAllowedAsDerivationOutputs() {
		// case 1: entities allowed to be added as outputs
		// the function should execute fine
		devClient.allowedAsDerivationOutputs(entities);

		// case 2: entity already belongsTo other derivations
		// mark derivation now, then the function should throw error
		devClient.createDerivation(entities, derivedAgentIRI, inputs);
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.allowedAsDerivationOutputs(entities));
		Assert.assertTrue(e.getMessage().contains("already part of another derivation"));

		// case 3: entity already has timestamp, is pure input
		// should throw error
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.allowedAsDerivationOutputs(inputs));
		Assert.assertTrue(e.getMessage().contains("have time instances"));
	}

	@Test
	public void testBulkCreateDerivations() {
		OntModel testKG = mockClient.getKnowledgeBase();
		List<List<String>> entitiesList = Arrays.asList(entities, entities2);
		List<List<String>> inputsList = Arrays.asList(inputs, inputs2);

		Resource derivationType = ResourceFactory.createResource(DerivationSparql.derivednamespace + "Derivation");

		// add timestamp to all pure inputs first
		devClient.addTimeInstance(inputsList.stream().flatMap(List::stream).collect(Collectors.toList()));
		// then create derivation, as all pure inputs already have timestamp in KG
		// the sub query that retrieves the pure inputs whose timestamp is missing will return empty results
		// but due to the optional clause, the insert clause should still be able to proceed to mark up derivation
		// thus if the below tests passes, the optional clause is tested automatically
		List<String> derivations = devClient.bulkCreateDerivations(entitiesList, agentIRIList, inputsList);
		for (int i = 0; i < derivations.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);

			Assert.assertEquals(derivationType, testKG.getIndividual(derivations.get(i)).getRDFType());

			for (String entity : entities) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(entity),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "belongsTo"),
						ResourceFactory.createResource(derivations.get(i))));
			}

			for (String input : inputs) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivations.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "isDerivedFrom"),
						ResourceFactory.createResource(input)));
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(input),
						ResourceFactory.createProperty(p_time + "hasTime")));
			}
		}

		// an instance cannot be part of two derivations
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(entitiesList, agentIRIList, inputsList));
		Assert.assertTrue(e.getMessage().contains("part of another derivation"));
		for (String d : derivations) {
			Assert.assertTrue(e.getMessage().contains(d));
		}
		for (String en : entitiesList.stream().flatMap(List::stream).collect(Collectors.toList())) {
			Assert.assertTrue(e.getMessage().contains(en));
		}

		// an instance cannot be marked belongsTo more than one derivation
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(
						Arrays.asList(entities3, entities3),
						Arrays.asList(derivedAgentIRI, derivedAgentIRI2),
						Arrays.asList(inputs1, inputs3)
				));
		Assert.assertTrue(e.getMessage().contains("Entity will be marked belongsTo more than one derivations"));
	}

	@Test
	public void testBulkCreateDerivationsExceptions() {
		// some errors for potential circular dependency will be detected at creation
		// NOTE however that there will be situations that the creation is okay in DerivationSparql
		// but circular dependency still exist, these can be detected by DerivationClient::validateDerivations
		// this is the reason that derivations are always validated behind-the-secenes when creating derivation in bulk by developer
		// check out DerivedQuantityClientTest::testValidateDerived and
		// DerivedQuantityClientTest::testBulkCreateDerivationsDetectCircularDependency

		// inputs cancelled out
		// e1 --> d1 --> i1. i1 --> d2 --> e1.
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(Arrays.asList(Arrays.asList(input1), Arrays.asList(entity1)),
						Arrays.asList(derivedAgentIRI, derivedAgentIRI2),
						Arrays.asList(Arrays.asList(entity1), Arrays.asList(input1))));
		Assert.assertTrue(e.getMessage().contains("All inputs are cancelled out"));

		// inputs cancelled out
		// e2 --> e1. e1 --> i1. i1 --> e2.
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(
						Arrays.asList(Arrays.asList(entity2), Arrays.asList(entity1), Arrays.asList(input1)),
						Arrays.asList(derivedAgentIRI, derivedAgentIRI2, derivedAgentIRI3),
						Arrays.asList(Arrays.asList(entity1), Arrays.asList(input1), Arrays.asList(entity2))));
		Assert.assertTrue(e.getMessage().contains("All inputs are cancelled out"));

		// inputs cancelled out
		// e2 --> e1. e1 --> i1, i2. i1, i2 --> e1.
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(
						Arrays.asList(Arrays.asList(entity2), Arrays.asList(entity1), Arrays.asList(input1, input2)),
						Arrays.asList(derivedAgentIRI, derivedAgentIRI2, derivedAgentIRI3),
						Arrays.asList(Arrays.asList(entity1), Arrays.asList(input1, input2), Arrays.asList(entity2))));
		Assert.assertTrue(e.getMessage().contains("All inputs are cancelled out"));

		// same IRI exist in both inputs and outputs
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(
						Arrays.asList(Arrays.asList(entity1)),
						Arrays.asList(derivedAgentIRI),
						Arrays.asList(Arrays.asList(entity1))));
		Assert.assertTrue(e.getMessage().contains("Intersection between inputs and outputs for the same derivation markup"));

		// entity will be belongsTo more than one derivation
		// e2 --> e1. e1 --> i1. e1 --> e2.
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devClient.bulkCreateDerivations(
						Arrays.asList(Arrays.asList(entity2), Arrays.asList(entity1), Arrays.asList(entity1)),
						Arrays.asList(derivedAgentIRI, derivedAgentIRI2, derivedAgentIRI3),
						Arrays.asList(Arrays.asList(entity1), Arrays.asList(input1), Arrays.asList(entity2))));
		Assert.assertTrue(e.getMessage().contains("Entity will be marked belongsTo more than one derivations"));
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

		List<String> derivations = devClient.bulkCreateDerivationsWithTimeSeries(entitiesList, agentIRIList, inputsList);
		for (int i = 0; i < derivations.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);

			Assert.assertEquals(derivationType, testKG.getIndividual(derivations.get(i)).getRDFType());

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
		List<Boolean> forAsyncUpdateFlagList = Arrays.asList(true, false);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");

		List<String> derivations = devClient.bulkCreateDerivationsAsync(entitiesList, agentIRIList,
				inputsList, forAsyncUpdateFlagList);
		for (int i = 0; i < derivations.size(); i++) {
			List<String> entities = entitiesList.get(i);
			List<String> inputs = inputsList.get(i);

			Assert.assertEquals(derivationType, testKG.getIndividual(derivations.get(i)).getRDFType());

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

			if (forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivations.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivations.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivations.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
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
		List<Boolean> forAsyncUpdateFlagList = Arrays.asList(false, false, true);

		List<String> derivations = devClient.bulkCreateMixedDerivations(entitiesListChain5, agentIRIListChain5,
				inputsListChain5, derivationTypeSequence, forAsyncUpdateFlagList);
		for (int i = 0; i < derivations.size(); i++) {
			List<String> entities = entitiesListChain5.get(i);
			List<String> inputs = inputsListChain5.get(i);

			Assert.assertEquals(derivationTypeResourceList.get(i),
					testKG.getIndividual(derivations.get(i)).getRDFType());

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

			if (forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivations.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivations.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivations.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
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
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1).toList().stream().map(i -> (String) i).collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2).toList().stream().map(i -> (String) i).collect(Collectors.toList())));
	}

	@Test
	public void testMapInstancesToAgentInputs() {
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
		JSONObject mappedInputs = devClient.mapInstancesToAgentInputs(Arrays.asList(input1, input2), derivedAgentIRI);
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1).toList().stream().map(i -> (String) i).collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2).toList().stream().map(i -> (String) i).collect(Collectors.toList())));
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
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
	}

	@Test
	public void testMapInstancesToAgentInputs_RdfType_RdfsSubClassOf() {
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
		JSONObject mappedInputs = devClient.mapInstancesToAgentInputs(Arrays.asList(input1, input2), derivedAgentIRI);
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
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
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1RdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2RdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
	}

	@Test
	public void testMapInstancesToAgentInputs_RdfType() {
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
		JSONObject mappedInputs = devClient.mapInstancesToAgentInputs(Arrays.asList(input1, input2), derivedAgentIRI);
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1RdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2RdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
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
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
	}

	@Test
	public void testMapInstancesToAgentInputs_RdfsSubClassOf() {
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
		JSONObject mappedInputs = devClient.mapInstancesToAgentInputs(Arrays.asList(input1, input2), derivedAgentIRI);
		Assert.assertTrue(equalLists(Arrays.asList(input1),
				mappedInputs.getJSONArray(input1ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
		Assert.assertTrue(equalLists(Arrays.asList(input2),
				mappedInputs.getJSONArray(input2ParentRdfType).toList().stream().map(i -> (String) i)
						.collect(Collectors.toList())));
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

	// below test case replaces the test case testGetDerivations()
	// as function getAllDerivationsInKG() replaces function getDerivations()
	@Test
	public void testGetAllDerivationsInKG_Derivation() {
		initOntoAgentInstances();
		for (int mode = 0; mode < compactEntititsList.size(); mode++) {
			List<List<String>> _entitiesList = compactEntititsList.get(mode);
			List<String> _agentIRIList = compactAgentIRIList.get(mode);
			List<String> _agentURLList = compactAgentURLList.get(mode);
			List<List<String>> _inputsList = compactInputsList.get(mode);

			List<String> derivationIRIs = devClient.bulkCreateDerivations(_entitiesList, _agentIRIList, _inputsList);
			devClient.addTimeInstance(derivationIRIs);
			OntModel testKG = mockClient.getKnowledgeBase();
			initRdfType(testKG);

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
		initOntoAgentInstances();
		for (int mode = 0; mode < compactEntititsList.size(); mode++) {
			List<List<String>> _entitiesList = compactEntititsList.get(mode);
			List<String> _agentIRIList = compactAgentIRIList.get(mode);
			List<String> _agentURLList = compactAgentURLList.get(mode);
			List<List<String>> _inputsList = compactInputsList.get(mode);
			List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, _entitiesList.size()).mapToObj(i -> true)
					.collect(Collectors.toList());

			List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(_entitiesList, _agentIRIList,
					_inputsList, _forAsyncUpdateFlagList);
			devClient.addTimeInstance(derivationIRIs);
			OntModel testKG = mockClient.getKnowledgeBase();
			initRdfType(testKG);

			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
			for (int i = 0; i < derivationIRIs.size(); i++) {
				Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRIs.get(i)).getRDFType());

				if (_forAsyncUpdateFlagList.get(i)) {
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
					String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
							ResourceFactory.createProperty(RDF.type.getURI()),
							ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
				} else {
					Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				}
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
	public void testGetAllDerivationsInKG_DerivationAsyn_NewInfo() {
		initOntoAgentInstances();
		String derivation1 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI0, inputs0, true);
		String derivation2 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI1,
				Arrays.asList(derivation1), true);
		String derivation3 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI2,
				Arrays.asList(derivation2), true);
		List<String> derivationIRIs = Arrays.asList(derivation1, derivation2, derivation3);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);
		// also need to write triples about agentURL to enable getAllDerivationsInKG()
		String hasOperation = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation";
		String hasHttpUrl = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl";
		testKG.add(ResourceFactory.createResource(derivedAgentIRI0), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentURL0 + "operation"));
		testKG.add(ResourceFactory.createResource(derivedAgentURL0 + "operation"),
				ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL0));
		testKG.add(ResourceFactory.createResource(derivedAgentIRI1), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentURL1 + "operation"));
		testKG.add(ResourceFactory.createResource(derivedAgentURL1 + "operation"),
				ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL1));
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentURL2 + "operation"));
		testKG.add(ResourceFactory.createResource(derivedAgentURL2 + "operation"),
				ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL2));

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
			Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(dIRI));
		}

		List<Derivation> derivations = devClient.getAllDerivationsInKG();
		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();
			switch (derivation.getAgentURL()) {
				// check four things for each derivation: inputs, outputs, directedUpstreams,
				// directedDownstreams
				case derivedAgentURL0:
					Assert.assertTrue(equalLists(inputs0, derivation.getAgentInputs()));
					Assert.assertTrue(derivation.getEntities().isEmpty());
					Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());
					Assert.assertTrue(
							equalLists(Arrays.asList(derivedAgentURL1), derivation.getDirectedDownstreams().stream()
									.map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				case derivedAgentURL1:
					Assert.assertTrue(derivation.getAgentInputs().isEmpty());
					Assert.assertTrue(derivation.getEntities().isEmpty());
					Assert.assertTrue(
							equalLists(Arrays.asList(derivedAgentURL0), derivation.getDirectedUpstreams().stream()
									.map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					Assert.assertTrue(
							equalLists(Arrays.asList(derivedAgentURL2), derivation.getDirectedDownstreams().stream()
									.map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				case derivedAgentURL2:
					Assert.assertTrue(derivation.getAgentInputs().isEmpty());
					Assert.assertTrue(derivation.getEntities().isEmpty());
					Assert.assertTrue(
							equalLists(Arrays.asList(derivedAgentURL1), derivation.getDirectedUpstreams().stream()
									.map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					Assert.assertTrue(derivation.getDirectedDownstreams().isEmpty());
					break;
				default:
					fail("Unexpected agentURL for derivation: " + derivation.getAgentURL());
			}
		}

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	// below test case replaces the test case testGetDerivations()
	// as function getAllDerivationsInKG() replaces function getDerivations()
	@Test
	public void testGetAllDerivationsInKG_MixedDerivation_NewInfo() {
		initOntoAgentInstances();
		String derivation1 = devClient.createDerivation(entities0, derivedAgentIRI0, inputs0);
		String derivation2 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI1,
				entities0, true);
		String derivation3 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI2,
				Arrays.asList(derivation2), true);
		List<String> derivationIRIs = Arrays.asList(derivation1, derivation2, derivation3);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);
		// also need to write triples about agentURL to enable getAllDerivationsInKG()
		String hasOperation = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation";
		String hasHttpUrl = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl";
		testKG.add(ResourceFactory.createResource(derivedAgentIRI1), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentURL1 + "operation"));
		testKG.add(ResourceFactory.createResource(derivedAgentURL1 + "operation"),
				ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL1));
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentURL2 + "operation"));
		testKG.add(ResourceFactory.createResource(derivedAgentURL2 + "operation"),
				ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL2));

		Resource derivationNormalType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "Derivation");
		Resource derivationAsynType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (String dIRI : derivationIRIs) {
			if (dIRI.contentEquals(derivation1)) {
				Assert.assertEquals(derivationNormalType, testKG.getIndividual(dIRI).getRDFType());
			} else {
				Assert.assertEquals(derivationAsynType, testKG.getIndividual(dIRI).getRDFType());
				Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(dIRI));
			}
		}

		List<Derivation> derivations = devClient.getAllDerivationsInKG();
		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();
			switch (derivation.getAgentURL()) {
				// check four things for each derivation: inputs, outputs, directedUpstreams,
				// directedDownstreams
				case derivedAgentURL0:
					Assert.assertTrue(equalLists(inputs0, derivation.getAgentInputs()));
					Assert.assertTrue(equalLists(entities0, derivation.getEntitiesIri()));
					Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());
					Assert.assertTrue(derivation.getDirectedDownstreams().isEmpty());
					// also check for immediate downstream derivations as outputs are not empty
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL1),
							derivation.getEntities().stream().map(en -> en.getInputOf())
									.flatMap(Collection::stream).distinct().collect(Collectors.toList()).stream()
									.map(d -> d.getAgentURL()).collect(Collectors.toList())));
					break;
				case derivedAgentURL1:
					Assert.assertTrue(equalLists(entities0, derivation.getAgentInputs()));
					Assert.assertTrue(derivation.getEntities().isEmpty());
					Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());
					Assert.assertTrue(
							equalLists(Arrays.asList(derivedAgentURL2), derivation.getDirectedDownstreams().stream()
									.map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				case derivedAgentURL2:
					Assert.assertTrue(derivation.getAgentInputs().isEmpty());
					Assert.assertTrue(derivation.getEntities().isEmpty());
					Assert.assertTrue(
							equalLists(Arrays.asList(derivedAgentURL1), derivation.getDirectedUpstreams().stream()
									.map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					Assert.assertTrue(derivation.getDirectedDownstreams().isEmpty());
					break;
				default:
					fail("Unexpected agentURL for derivation: " + derivation.getAgentURL());
			}
		}

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	// below test case replaces the test case testGetDerivations()
	// as function getAllDerivationsInKG() replaces function getDerivations()
	// two async derivations (new info) <isDerivedFrom> the same upstream sync
	// derivation
	@Test
	public void testGetAllDerivationsInKG_MixedDerivation_NewInfo_Two() {
		initOntoAgentInstances();
		String derivation1 = devClient.createDerivation(entities0, derivedAgentIRI0, inputs0);
		String derivation2 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI1,
				entities0, true);
		String derivation3 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI2,
				entities0, true);
		List<String> derivationIRIs = Arrays.asList(derivation1, derivation2, derivation3);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);
		// also need to write triples about agentURL to enable getAllDerivationsInKG()
		String hasOperation = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation";
		String hasHttpUrl = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl";
		testKG.add(ResourceFactory.createResource(derivedAgentIRI1), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentURL1 + "operation"));
		testKG.add(ResourceFactory.createResource(derivedAgentURL1 + "operation"),
				ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL1));
		testKG.add(ResourceFactory.createResource(derivedAgentIRI2), ResourceFactory.createProperty(hasOperation),
				ResourceFactory.createResource(derivedAgentURL2 + "operation"));
		testKG.add(ResourceFactory.createResource(derivedAgentURL2 + "operation"),
				ResourceFactory.createProperty(hasHttpUrl),
				ResourceFactory.createResource(derivedAgentURL2));

		Resource derivationNormalType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "Derivation");
		Resource derivationAsynType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (String dIRI : derivationIRIs) {
			if (dIRI.contentEquals(derivation1)) {
				Assert.assertEquals(derivationNormalType, testKG.getIndividual(dIRI).getRDFType());
			} else {
				Assert.assertEquals(derivationAsynType, testKG.getIndividual(dIRI).getRDFType());
				Assert.assertEquals(StatusType.REQUESTED, devClient.getStatusType(dIRI));
			}
		}

		List<Derivation> derivations = devClient.getAllDerivationsInKG();
		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();
			switch (derivation.getAgentURL()) {
				// check four things for each derivation: inputs, outputs, directedUpstreams,
				// directedDownstreams
				case derivedAgentURL0:
					Assert.assertTrue(equalLists(inputs0, derivation.getAgentInputs()));
					Assert.assertTrue(equalLists(entities0, derivation.getEntitiesIri()));
					Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());
					Assert.assertTrue(derivation.getDirectedDownstreams().isEmpty());
					// also check for immediate downstream derivations as outputs are not empty
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL1, derivedAgentURL2),
							derivation.getEntities().stream().map(en -> en.getInputOf())
									.flatMap(Collection::stream).distinct().collect(Collectors.toList()).stream()
									.map(d -> d.getAgentURL()).collect(Collectors.toList())));
					break;
				case derivedAgentURL1:
					Assert.assertTrue(equalLists(entities0, derivation.getAgentInputs()));
					Assert.assertTrue(derivation.getEntities().isEmpty());
					Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());
					Assert.assertTrue(derivation.getDirectedDownstreams().isEmpty());
					break;
				case derivedAgentURL2:
					Assert.assertTrue(equalLists(entities0, derivation.getAgentInputs()));
					Assert.assertTrue(derivation.getEntities().isEmpty());
					Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());
					Assert.assertTrue(derivation.getDirectedDownstreams().isEmpty());
					break;
				default:
					fail("Unexpected agentURL for derivation: " + derivation.getAgentURL());
			}
		}

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_Fragmented_Derivation() {
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		for (int cas = 0; cas < fragmentedEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = fragmentedEntititsList.get(cas);
			List<String> _agentIRIList = fragmentedAgentIRIList.get(cas);
			List<String> _agentURLList = fragmentedAgentURLList.get(cas);
			List<List<String>> _inputsList = fragmentedInputsList.get(cas);

			List<String> derivationIRIs = devClient.bulkCreateDerivations(_entitiesList, _agentIRIList, _inputsList);
			devClient.addTimeInstance(derivationIRIs);
			OntModel testKG = mockClient.getKnowledgeBase();
			initRdfType(testKG);

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
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		for (int cas = 0; cas < fragmentedEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = fragmentedEntititsList.get(cas);
			List<String> _agentIRIList = fragmentedAgentIRIList.get(cas);
			List<String> _agentURLList = fragmentedAgentURLList.get(cas);
			List<List<String>> _inputsList = fragmentedInputsList.get(cas);
			List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, _entitiesList.size()).mapToObj(i -> false)
					.collect(Collectors.toList());

			List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(_entitiesList, _agentIRIList,
					_inputsList, _forAsyncUpdateFlagList);
			devClient.addTimeInstance(derivationIRIs);
			OntModel testKG = mockClient.getKnowledgeBase();
			initRdfType(testKG);

			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
			for (int i = 0; i < derivationIRIs.size(); i++) {
				Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRIs.get(i)).getRDFType());

				if (_forAsyncUpdateFlagList.get(i)) {
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
					String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
							ResourceFactory.createProperty(RDF.type.getURI()),
							ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
				} else {
					Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				}
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
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		for (int cas = 0; cas < chainEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = chainEntititsList.get(cas);
			List<String> _agentIRIList = chainAgentIRIList.get(cas);
			List<String> _agentURLList = chainAgentURLList.get(cas);
			List<List<String>> _inputsList = chainInputsList.get(cas);

			List<String> derivationIRIs = devClient.bulkCreateDerivations(_entitiesList, _agentIRIList, _inputsList);
			devClient.addTimeInstance(derivationIRIs);
			OntModel testKG = mockClient.getKnowledgeBase();
			initRdfType(testKG);

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
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		for (int cas = 0; cas < chainEntititsList.size(); cas++) {
			List<List<String>> _entitiesList = chainEntititsList.get(cas);
			List<String> _agentIRIList = chainAgentIRIList.get(cas);
			List<String> _agentURLList = chainAgentURLList.get(cas);
			List<List<String>> _inputsList = chainInputsList.get(cas);
			List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, _entitiesList.size()).mapToObj(i -> true)
					.collect(Collectors.toList());

			List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(_entitiesList, _agentIRIList,
					_inputsList, _forAsyncUpdateFlagList);
			devClient.addTimeInstance(derivationIRIs);
			OntModel testKG = mockClient.getKnowledgeBase();
			initRdfType(testKG);

			Resource derivationType = ResourceFactory
					.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
			for (int i = 0; i < derivationIRIs.size(); i++) {
				Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRIs.get(i)).getRDFType());

				if (_forAsyncUpdateFlagList.get(i)) {
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
					String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
					Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
							ResourceFactory.createProperty(RDF.type.getURI()),
							ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
				} else {
					Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
						ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				}
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
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		// tree case 1 has structure: d3 --> d1, d3 --> d2, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListTree1, agentIRIListTree1,
				inputsListTree1);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

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
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, entitiesListTree1.size()).mapToObj(i -> true)
				.collect(Collectors.toList());
		// tree case 1 has structure: d3 --> d1, d3 --> d2, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListTree1, agentIRIListTree1,
				inputsListTree1, _forAsyncUpdateFlagList);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (int i = 0; i < derivationIRIs.size(); i++) {
			Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRIs.get(i)).getRDFType());

			if (_forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
			}
		}

		assertTestGetRootAndAllUpstreamDerivations_Tree1(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG1_Derivation() {
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		// DAG case 1 has structure: d1 --> d0, d2 --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListDAG1, agentIRIListDAG1,
				inputsListDAG1);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

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
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, entitiesListDAG1.size()).mapToObj(i -> false)
				.collect(Collectors.toList());
		// DAG case 1 has structure: d1 --> d0, d2 --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListDAG1, agentIRIListDAG1,
				inputsListDAG1, _forAsyncUpdateFlagList);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (int i = 0; i < derivationIRIs.size(); i++) {
			Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRIs.get(i)).getRDFType());

			if (_forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
			}
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG1(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG2_Derivation() {
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		// DAG case 2 has structure: d3 --> (d1, d2) --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListDAG2, agentIRIListDAG2, inputsListDAG2);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

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
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, entitiesListDAG2.size()).mapToObj(i -> true)
				.collect(Collectors.toList());
		// DAG case 2 has structure: d3 --> (d1, d2) --> d0, no connection between d1/d2
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListDAG2, agentIRIListDAG2,
				inputsListDAG2, _forAsyncUpdateFlagList);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (int i = 0; i < derivationIRIs.size(); i++) {
			Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRIs.get(i)).getRDFType());

			if (_forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
			}
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG2(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG3_Derivation() {
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATION);
		// DAG case 3 has structure: (d2, d4) --> d0, both d2 and d4 <isDerivedFrom> i3
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListDAG3, agentIRIListDAG3,
				inputsListDAG3);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "Derivation");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG3(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetRootAndAllUpstreamDerivations_DAG3_DerivationAsyn() {
		initOntoAgentInstances();
		List<String> derivationTypeList = Arrays
				.asList(DerivationSparql.derivednamespace + DerivationSparql.DERIVATIONASYN);
		List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, entitiesListDAG3.size()).mapToObj(i -> false)
				.collect(Collectors.toList());
		// DAG case 3 has structure: (d2, d4) --> d0, both d2 and d4 <isDerivedFrom> i3
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListDAG3, agentIRIListDAG3,
				inputsListDAG3, _forAsyncUpdateFlagList);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "DerivationAsyn");
		for (int i = 0; i < derivationIRIs.size(); i++) {
			Assert.assertEquals(derivationType, testKG.getIndividual(derivationIRIs.get(i)).getRDFType());

			if (_forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
			}
		}

		assertTestGetRootAndAllUpstreamDerivations_DAG3(devClient, derivationIRIs, derivationTypeList);

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetAllDerivationsInKG_Derivation_DAG4_Connectivity() {
		initOntoAgentInstances();
		// this function mainly tests the connectivity of the cached derivations in DAG4
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListDAG4, agentIRIListDAG4, inputsListDAG4);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		Resource derivationType = ResourceFactory
				.createResource(DerivationSparql.derivednamespace + "Derivation");
		for (String dIRI : derivationIRIs) {
			Assert.assertEquals(derivationType, testKG.getIndividual(dIRI).getRDFType());
		}

		// check inputs/outputs
		assertTestGetAllDerivationsInKG(devClient, derivationIRIs, entitiesListDAG4, agentURLListDAG4, inputsListDAG4);

		// check connectivity
		List<Derivation> derivations = devClient.getAllDerivationsInKG();
		// 1. from d0 to d5
		Derivation d0 = derivations.stream().filter(d -> d.getAgentURL().contentEquals(derivedAgentURL0)).findFirst()
				.get();
		assertD0(d0);
		// d0 should have two downstream derivations d2/d4 in DAG4
		Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL2, derivedAgentURL4),
				d0.getEntities().get(0).getInputOf().stream().map(d -> d.getAgentURL()).collect(Collectors.toList())));
		// in DAG4, both d2/d4 should only have one downstream derivation: d5
		d0.getEntities().get(0).getInputOf().stream().forEach(d -> {
			Assert.assertEquals(Arrays.asList(derivedAgentURL5),
					d.getEntities().stream().filter(e -> e.getInputOf() != null).map(e -> e.getInputOf())
							.flatMap(Collection::stream).collect(Collectors.toList())
							.stream().map(dd -> dd.getAgentURL()).distinct().collect(Collectors.toList()));
		});
		// 2. from d5 to d0
		Derivation d5 = derivations.stream().filter(d -> d.getAgentURL().contentEquals(derivedAgentURL5)).findFirst()
				.get();
		assertD5(d5);
		// d5 should have two upstream derivations d2/d4 in DAG4
		Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL2, derivedAgentURL4),
				d5.getInputsWithBelongsTo().stream().map(d -> d.getAgentURL()).distinct()
						.collect(Collectors.toList())));
		// in DAG4, both d2/d4 should only have one upstream derivation: d0
		d5.getInputsWithBelongsTo().stream().forEach(d -> {
			Assert.assertEquals(Arrays.asList(derivedAgentURL0), d.getInputsWithBelongsTo().stream()
					.map(ud -> ud.getAgentURL()).distinct().collect(Collectors.toList()));
		});

		devClient.dropAllDerivations();
		devClient.dropAllTimestamps();
		List<Derivation> allDerivations = devClient.getAllDerivationsInKG();
		Assert.assertEquals(0, allDerivations.size());
	}

	@Test
	public void testGetDerivation() {
		initOntoAgentInstances();
		List<String> derivationIRIs = devClient.bulkCreateDerivations(entitiesListDAG5, agentIRIListDAG5, inputsListDAG5);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		for (int i = 0; i < derivationIRIs.size(); i++) {
			List<Derivation> ds = devClient.getAllDerivationsInKG();
			Derivation derivation = devClient.getDerivation(derivationIRIs.get(i));
			// inputs
			Assert.assertTrue(equalLists(inputsListDAG5.get(i), derivation.getAgentInputs()));

			// outputs
			Assert.assertTrue(equalLists(entitiesListDAG5.get(i), derivation.getEntitiesIri()));

			// agent url
			Assert.assertEquals(agentURLListDAG5.get(i), derivation.getAgentURL());

			// no upstream derivation cached
			Assert.assertTrue(derivation.getInputsWithBelongsTo().isEmpty());
			Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());

			// no downstream derivation cached
			Assert.assertTrue(collectDistinctImmediateDownstreamDerivations(derivation).isEmpty());
			Assert.assertTrue(derivation.getDirectedDownstreams().isEmpty());
		}
	}

	@Test
	public void testGetDownstreamDerivationForNewInfo() {
		// create three async derivations for new information
		String d0 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI, inputs, true);
		String d1 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI, Arrays.asList(d0), true);
		String d2 = devClient.createDerivationAsync(new ArrayList<>(), derivedAgentIRI2, Arrays.asList(d0), true);
		Map<String, String> map = devClient.getDownstreamDerivationForNewInfo(d0);
		Assert.assertTrue(map.get(d1).contentEquals(derivedAgentIRI));
		Assert.assertTrue(map.get(d2).contentEquals(derivedAgentIRI2));
	}

	@Test
	public void testGetUpstreamDerivationsNeedUpdate() {
		// create DAG5 structure, d0 sync with timeseries, d1/d2 sync, d3/d4/d5 async
		List<String> ds = devClient.bulkCreateMixedDerivations(entitiesListDAG5, agentIRIListDAG5, inputsListDAG5,
				Arrays.asList(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES,
						DerivationSparql.ONTODERIVATION_DERIVATION, DerivationSparql.ONTODERIVATION_DERIVATION,
						DerivationSparql.ONTODERIVATION_DERIVATIONASYN,
						DerivationSparql.ONTODERIVATION_DERIVATIONASYN,
						DerivationSparql.ONTODERIVATION_DERIVATIONASYN),
				Arrays.asList(false, false, false, false, true, false));
		// timestamp should already be added to all pure inputs with current timestamp in bulkCreateMixedDerivations
		// add timestamp to all derivation with 0 timestamp, also mark them as Requested
		// --> this is the same as the status of derivations after call
		// unifiedUpdateDerivations
		devClient.addTimeInstance(ds);
		for (String d : ds) {
			devClient.markAsRequested(d);
		}

		for (int i = 0; i < ds.size(); i++) {
			String agentURL = agentURLListDAG5.get(i);
			String derivationIRI = ds.get(i);
			Map<String, List<String>> upstreamDerivationsNeedUpdate = devClient
					.getUpstreamDerivationsNeedUpdate(derivationIRI);
			switch (agentURL) {
				// check that the Iri of the upstream derivations need update are correct
				case derivedAgentURL0:
					Assert.assertTrue(upstreamDerivationsNeedUpdate.isEmpty());
					break;
				case derivedAgentURL1:
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate.containsKey(DerivationSparql.ONTODERIVATION_DERIVATION));
					Assert.assertTrue(equalLists(Arrays.asList(ds.get(0)),
							upstreamDerivationsNeedUpdate
									.get(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES)));
					break;
				case derivedAgentURL2:
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate.containsKey(DerivationSparql.ONTODERIVATION_DERIVATION));
					Assert.assertTrue(equalLists(Arrays.asList(ds.get(0)),
							upstreamDerivationsNeedUpdate
									.get(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES)));
					break;
				case derivedAgentURL3:
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate
									.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES));
					Assert.assertTrue(equalLists(Arrays.asList(ds.get(1), ds.get(2)),
							upstreamDerivationsNeedUpdate.get(DerivationSparql.ONTODERIVATION_DERIVATION)));
					break;
				case derivedAgentURL4:
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONASYN));
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate.containsKey(DerivationSparql.ONTODERIVATION_DERIVATION));
					Assert.assertTrue(equalLists(Arrays.asList(ds.get(0)),
							upstreamDerivationsNeedUpdate
									.get(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES)));
					break;
				case derivedAgentURL5:
					Assert.assertTrue(
							!upstreamDerivationsNeedUpdate
									.containsKey(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES));
					Assert.assertTrue(equalLists(Arrays.asList(ds.get(2)),
							upstreamDerivationsNeedUpdate.get(DerivationSparql.ONTODERIVATION_DERIVATION)));
					Assert.assertTrue(equalLists(Arrays.asList(ds.get(4)),
							upstreamDerivationsNeedUpdate.get(DerivationSparql.ONTODERIVATION_DERIVATIONASYN)));
					break;
				default:
					fail("Unexpected agentURL for derivation: " + agentURL);
			}
		}
	}

	@Test
	public void testGetAllImmediateUpstreamDerivations() {
		initOntoAgentInstances();
		List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, entitiesListDAG5.size()).mapToObj(i -> true)
					.collect(Collectors.toList());
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListDAG5, agentIRIListDAG5,
				inputsListDAG5, _forAsyncUpdateFlagList);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			String agentURL = agentURLListDAG5.get(i);
			if (_forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
			}
			List<Derivation> allImmediateUpstreamDerivations = devClient
					.getAllImmediateUpstreamDerivations(derivationIRI);
			// check NONE of these derivations connect to any other derivations
			Assert.assertTrue(
					allImmediateUpstreamDerivations.stream().allMatch(d -> d.getInputsWithBelongsTo().isEmpty()));
			Assert.assertTrue(
					allImmediateUpstreamDerivations.stream().allMatch(d -> d.getDirectedUpstreams().isEmpty()));
			Assert.assertTrue(
					allImmediateUpstreamDerivations.stream().allMatch(d -> d.getDirectedDownstreams().isEmpty()));
			Assert.assertTrue(
					allImmediateUpstreamDerivations.stream()
							.allMatch(d -> d.getEntities().stream().allMatch(en -> !en.isInputToDerivation())));
			switch (agentURL) {
				// check that the Iri of the upstream derivations are correct
				case derivedAgentURL0:
					Assert.assertTrue(allImmediateUpstreamDerivations.isEmpty());
					break;
				case derivedAgentURL1:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL0), allImmediateUpstreamDerivations
							.stream().map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				case derivedAgentURL2:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL0), allImmediateUpstreamDerivations
							.stream().map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				case derivedAgentURL3:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL1, derivedAgentURL2),
							allImmediateUpstreamDerivations
									.stream().map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				case derivedAgentURL4:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL0), allImmediateUpstreamDerivations
							.stream().map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				case derivedAgentURL5:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL2, derivedAgentURL4),
							allImmediateUpstreamDerivations
									.stream().map(d -> d.getAgentURL()).distinct().collect(Collectors.toList())));
					break;
				default:
					fail("Unexpected agentURL for derivation: " + agentURL);
			}
		}
	}

	@Test
	public void testGetDerivationWithImmediateDownstream() {
		initOntoAgentInstances();
		// first test with DAG5 structure
		List<Boolean> _forAsyncUpdateFlagList = IntStream.range(0, entitiesListDAG5.size()).mapToObj(i -> false)
				.collect(Collectors.toList());
		List<String> derivationIRIs = devClient.bulkCreateDerivationsAsync(entitiesListDAG5, agentIRIListDAG5,
				inputsListDAG5, _forAsyncUpdateFlagList);
		devClient.addTimeInstance(derivationIRIs);
		OntModel testKG = mockClient.getKnowledgeBase();
		initRdfType(testKG);

		String d0 = new String();

		for (int i = 0; i < derivationIRIs.size(); i++) {
			String derivationIRI = derivationIRIs.get(i);
			String agentURL = agentURLListDAG5.get(i);
			if (_forAsyncUpdateFlagList.get(i)) {
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
				String statusIRI = testKG.getProperty(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")).getObject().toString();
				Assert.assertTrue(testKG.contains(ResourceFactory.createResource(statusIRI),
						ResourceFactory.createProperty(RDF.type.getURI()),
						ResourceFactory.createResource(DerivationSparql.derivednamespace + "Requested")));
			} else {
				Assert.assertTrue(!testKG.contains(ResourceFactory.createResource(derivationIRIs.get(i)),
					ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus")));
			}
			Derivation derivation = devClient.getDerivationWithImmediateDownstream(derivationIRI);
			// check the constructed derivation from queried results does NOT have
			// downstream derivations other than the immediate ones; also it does NOT have
			// the upstream derivations
			Assert.assertTrue(collectDistinctImmediateDownstreamDerivations(derivation).stream()
					.allMatch(d -> d.getEntities().stream().allMatch(e -> !e.isInputToDerivation())));
			Assert.assertTrue(derivation.getDirectedDownstreams().stream().allMatch(d -> d.getEntitiesIri().isEmpty()));
			Assert.assertTrue(derivation.getInputsWithBelongsTo().isEmpty());
			Assert.assertTrue(derivation.getDirectedUpstreams().isEmpty());
			switch (agentURL) {
				// check that the Iri of the downstream derivations are correct
				case derivedAgentURL0:
					d0 = derivationIRI;
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL1, derivedAgentURL2, derivedAgentURL4),
							collectDistinctImmediateDownstreamDerivations(derivation).stream().map(d -> d.getAgentURL())
									.collect(Collectors.toList())));
					break;
				case derivedAgentURL1:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL3),
							collectDistinctImmediateDownstreamDerivations(derivation).stream().map(d -> d.getAgentURL())
									.collect(Collectors.toList())));
					break;
				case derivedAgentURL2:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL3, derivedAgentURL5),
							collectDistinctImmediateDownstreamDerivations(derivation).stream().map(d -> d.getAgentURL())
									.collect(Collectors.toList())));
					break;
				case derivedAgentURL3:
					Assert.assertTrue(collectDistinctImmediateDownstreamDerivations(derivation).isEmpty());
					break;
				case derivedAgentURL4:
					Assert.assertTrue(equalLists(Arrays.asList(derivedAgentURL5),
							collectDistinctImmediateDownstreamDerivations(derivation).stream().map(d -> d.getAgentURL())
									.collect(Collectors.toList())));
					break;
				case derivedAgentURL5:
					Assert.assertTrue(collectDistinctImmediateDownstreamDerivations(derivation).isEmpty());
					break;
				default:
					fail("Unexpected agentURL for derivation: " + agentURL);
			}
		}

		// add a directed downstream derivation to d0, this should also be cached
		// the ontoagent instance of this random agent should also be added to let it be cached
		String agentIRI_random = "http://agent_iri_" + UUID.randomUUID().toString();
		String agentURL_random = "http://agent_url_" + UUID.randomUUID().toString();
		devClient.createOntoAgentInstance(agentIRI_random, agentURL_random, Arrays.asList("http://random_i"), Arrays.asList("http://random_o"));
		String derivation_random = devClient.bulkCreateDerivationsAsync(Arrays.asList(new ArrayList<String>()),
				Arrays.asList(agentIRI_random), Arrays.asList(Arrays.asList(d0)), Arrays.asList(false))
				.get(0);
		devClient.addTimeInstance(derivation_random);
		Derivation d_0 = devClient.getDerivationWithImmediateDownstream(d0);
		List<Derivation> ds = collectDistinctImmediateDownstreamDerivations(d_0);
		ds.addAll(d_0.getDirectedDownstreams());
		Assert.assertTrue(
				equalLists(Arrays.asList(derivedAgentURL1, derivedAgentURL2, derivedAgentURL4, agentURL_random),
						ds.stream().map(d -> d.getAgentURL())
								.collect(Collectors.toList())));

		// mark d0 to have status that associated with new derived instances, these
		// information should also be cached
		String statusIRI = "http://status_" + UUID.randomUUID().toString();
		testKG.add(ResourceFactory.createResource(d0),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasStatus"),
				ResourceFactory.createResource(statusIRI));
		testKG.add(ResourceFactory.createResource(statusIRI), RDF.type,
				ResourceFactory.createResource(DerivationSparql.derivednamespace + "Finished"));
		String new_iri_1 = "http://iri_" + UUID.randomUUID().toString();
		String new_iri_2 = "http://iri_" + UUID.randomUUID().toString();
		String new_iri_3 = "http://iri_" + UUID.randomUUID().toString();
		testKG.add(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasNewDerivedIRI"),
				ResourceFactory.createResource(new_iri_1));
		testKG.add(ResourceFactory.createResource(new_iri_1), RDF.type,
				ResourceFactory.createResource(new_iri_1 + "/rdftype"));
		testKG.add(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasNewDerivedIRI"),
				ResourceFactory.createResource(new_iri_2));
		testKG.add(ResourceFactory.createResource(new_iri_2), RDF.type,
				ResourceFactory.createResource(new_iri_2 + "/rdftype"));
		testKG.add(ResourceFactory.createResource(statusIRI),
				ResourceFactory.createProperty(DerivationSparql.derivednamespace + "hasNewDerivedIRI"),
				ResourceFactory.createResource(new_iri_3));
		testKG.add(ResourceFactory.createResource(new_iri_3), RDF.type,
				ResourceFactory.createResource(new_iri_3 + "/rdftype"));
		d_0 = devClient.getDerivationWithImmediateDownstream(d0);
		ds = collectDistinctImmediateDownstreamDerivations(d_0);
		ds.addAll(d_0.getDirectedDownstreams());
		Assert.assertTrue(
				equalLists(Arrays.asList(derivedAgentURL1, derivedAgentURL2, derivedAgentURL4, agentURL_random),
						ds.stream().map(d -> d.getAgentURL())
								.collect(Collectors.toList())));
		Assert.assertTrue(d_0.getStatus().getStatusIri().equals(statusIRI));
		Assert.assertTrue(d_0.getStatus().getStatusRdfType().equals(DerivationSparql.derivednamespace + "Finished"));
		Assert.assertTrue(equalLists(Arrays.asList(new_iri_1, new_iri_2, new_iri_3),
				d_0.getStatus().getNewDerivedIRI().stream().map(e -> e.getIri()).collect(Collectors.toList())));
		Assert.assertTrue(equalLists(
				Arrays.asList(new_iri_1 + "/rdftype", new_iri_2 + "/rdftype", new_iri_3 + "/rdftype"),
				d_0.getStatus().getNewDerivedIRI().stream().map(e -> e.getRdfType()).collect(Collectors.toList())));
	}

	////////////////////////////////////////////////////////////
	// Below are utility functions to reduce code-duplication //
	////////////////////////////////////////////////////////////
	public void initRdfType(OntModel testKG) {
		for (String i : allInstances) {
			testKG.add(ResourceFactory.createResource(i), RDF.type, ResourceFactory.createResource(i + "/rdftype"));
		}
	}

	public void initOntoAgentInstances() {
		devClient.createOntoAgentInstance(derivedAgentIRI0, derivedAgentURL0, inputs0, entities0);
		devClient.createOntoAgentInstance(derivedAgentIRI1, derivedAgentURL1, inputs1, entities1);
		devClient.createOntoAgentInstance(derivedAgentIRI2, derivedAgentURL2, inputs2, entities2);
		devClient.createOntoAgentInstance(derivedAgentIRI3, derivedAgentURL3, inputs3, entities3);
		devClient.createOntoAgentInstance(derivedAgentIRI4, derivedAgentURL4, inputs4, entities4);
		devClient.createOntoAgentInstance(derivedAgentIRI5, derivedAgentURL5, inputs5, entities5);
	}

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

	public void assertTestGetRootAndAllUpstreamDerivations_DAG3(DerivationSparql devClient,
			List<String> derivationIRIs, List<String> derivationTypeList) {
		// DAG case 3 has structure: (d2, d4) --> d0, both d2 and d4 <isDerivedFrom> i3

		for (String derivationIRI : derivationIRIs) {
			List<Derivation> derivations = devClient.getRootAndAllTargetUpstreamDerivations(derivationIRI,
					derivationTypeList);
			Derivation derivation = derivations.stream().filter(d -> d.getIri().contentEquals(derivationIRI))
					.findFirst().get();

			// for those derivation type that not provided in the (target)derivationTypeList
			// within the argument of this function, make sure NO instances of derivation
			// should be pulled when getRootAndAllTargetUpstreamDerivations, this is
			// specific to this function as it's assumed that none of the derivations
			// outside of the (target)derivationTypeList will be created in the test case
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
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL2)) {
				// when d2 is root, both d2 and d0 should be cached and connected to each other,
				// but NO information about d4 should be cached
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
			} else if (derivation.getAgentURL().contentEquals(derivedAgentURL4)) {
				// when d4 is root, both d4 and d0 should be cached and connected to each other,
				// but NO information about d2 should be cached
				Assert.assertEquals(2, derivations.size());
				assertD4(derivation);
				// Make sure no information about downstream are cached
				assertNoImmediateDownstreamDerivation(derivation);
				// Make sure only upstream derivation is d0, and d0 is connected to d2
				assertHasOnlyImmediateUpstreamD0(derivation);
				List<Derivation> _upD = collectDistinctImmediateUpstreamDerivations(derivation);
				Assert.assertEquals(1, _upD.size());
				assertD0(_upD.get(0));
				assertHasOnlyImmediateDownstreamD4(_upD.get(0));
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

	public void assertD4(Derivation d4) {
		Assert.assertEquals(derivedAgentURL4, d4.getAgentURL());
		Assert.assertTrue(equalLists(inputs4, d4.getAgentInputs()));
		Assert.assertTrue(equalLists(entities4, d4.getEntitiesIri()));
	}

	public void assertD5(Derivation d5) {
		Assert.assertEquals(derivedAgentURL5, d5.getAgentURL());
		Assert.assertTrue(equalLists(inputs5, d5.getAgentInputs()));
		Assert.assertTrue(equalLists(entities5, d5.getEntitiesIri()));
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

	public void assertHasOnlyImmediateDownstreamD4(Derivation d) {
		List<Derivation> derivations = collectDistinctImmediateDownstreamDerivations(d);
		Assert.assertEquals(1, derivations.size());
		Derivation d4 = derivations.get(0);
		assertD4(d4);
		assertNoImmediateDownstreamDerivation(d4);
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
				.flatMap(Collection::stream).distinct().collect(Collectors.toList());
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
