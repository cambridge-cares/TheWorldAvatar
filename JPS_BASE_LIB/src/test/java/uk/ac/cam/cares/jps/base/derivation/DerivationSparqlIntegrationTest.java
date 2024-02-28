package uk.ac.cam.cares.jps.base.derivation;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import uk.ac.cam.cares.jps.base.BlazegraphContainer;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@Testcontainers
class DerivationSparqlIntegrationTest {
	private static RemoteStoreClient storeClient;
	private static DerivationSparql devSparql;
	private static final String agentIRI = "http://agentIRI";
	private static final String derivationBaseUrl = "http://derivation/";

	@Container
	private static final BlazegraphContainer blazegraph = new BlazegraphContainer();

	@BeforeAll
	static void initialise() {
		// initialise all variables to be used
		storeClient = blazegraph.getRemoteStoreClient();
		devSparql = new DerivationSparql(storeClient, derivationBaseUrl);
	}

	@Test
	void testMarkAsError() {
		// this tests writing exception to triple store
		List<String> outputs = new ArrayList<>(Arrays.asList("http://" + UUID.randomUUID().toString()));
		List<String> inputs = new ArrayList<>(Arrays.asList("http://" + UUID.randomUUID().toString()));
		String derivation = devSparql.createDerivation(outputs, agentIRI, inputs);
		// add timestamp to derivations, the timestamp of inputs is automatically added
		devSparql.addTimeInstance(derivation);
		// as all inputs' timestamp will be current timestamp, the derivation should be
		// deemed as outdated
		devSparql.markAsRequestedIfOutdated(derivation);

		// get an exception by checking if the inputs are allowed to be outputs for
		// other derivations
		JPSRuntimeException exc = Assertions.assertThrows(JPSRuntimeException.class,
				() -> devSparql.allowedAsDerivationOutputs(inputs));

		String excComment = devSparql.markAsError(derivation, exc);
		System.out.println(excComment);
		Assertions.assertEquals(StatusType.ERROR, devSparql.getStatusType(derivation));
		String askQuery = String.format(
				"SELECT ?c WHERE { <%s> <%s>/<%s> ?c }", derivation, DerivationSparql.derivednamespace + "hasStatus",
				RDFS.COMMENT.toString());
		Assertions.assertEquals(excComment, storeClient.executeQuery(askQuery).getJSONObject(0).getString("c"));

		Assertions.assertTrue(excComment.contains(exc.getClass().toString()));
		Assertions.assertTrue(excComment.contains(exc.getMessage()));
		for (StackTraceElement st : exc.getStackTrace()) {
			Assertions.assertTrue(excComment.contains(st.toString()));
		}
	}

	/**
	 * This method serves as integration test for method
	 * DerivationSparql::updateDerivationIfHttpPostAvailable.
	 */
	@Test
	void testReconnectSyncDerivation() {
		String derivedAgentIRI = "http://" + UUID.randomUUID().toString();
		List<String> inputs = Arrays.asList("http://" + UUID.randomUUID().toString(),
				"http://" + UUID.randomUUID().toString());
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
		String derivation = devSparql.createDerivation(oldInstances, derivedAgentIRI, inputs);
		devSparql.addTimeInstance(derivation); // timestamp initialised as 0
		// timestamp for all inputs should already be added automatically when
		// createDerivation

		// test if derivation was created correctly agent
		Assertions.assertTrue(
				checkTripleExist(derivation, DerivationSparql.derivednamespace + "isDerivedUsing", derivedAgentIRI));

		// outputs
		for (String instance : oldInstances) {
			Assertions.assertTrue(checkTripleExist(instance, DerivationSparql.derivednamespace + "belongsTo", derivation));
		}
		// inputs
		for (String input : inputs) {
			Assertions.assertTrue(checkTripleExist(derivation, DerivationSparql.derivednamespace + "isDerivedFrom", input));
		}
		// new instance set 1 should NOT be created
		newInstanceMap1.keySet().stream().forEach(instance -> {
			Assertions.assertTrue(!checkInstanceHasRdfType(instance));
		});
		// new instance set 2 should NOT be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assertions.assertTrue(!checkInstanceHasRdfType(instance));
		});

		// case 1: derivation is outdated, now delete and add new instances should work
		long retrievedInputsAt = Instant.now().getEpochSecond();
		boolean triplesChanged = devSparql.reconnectSyncDerivation(derivation, newInstanceMap1, newTriples1,
				retrievedInputsAt);
		Assertions.assertTrue(triplesChanged);
		// all old outputs should be deleted
		for (String instance : oldInstances) {
			Assertions.assertTrue(!checkInstanceHasRdfType(instance));
		}
		// new outputs should be connected to this derivation, and its downstreams
		newInstanceMap1.forEach((instance, dds) -> {
			Assertions.assertTrue(checkTripleExist(instance, DerivationSparql.derivednamespace + "belongsTo", derivation));
			Assertions.assertTrue(checkInstanceHasRdfType(instance));
			if (!dds.isEmpty()) {
				dds.stream().forEach(dd -> {
					Assertions.assertTrue(
							checkTripleExist(dd, DerivationSparql.derivednamespace + "isDerivedFrom", instance));
				});
			}
		});
		// timestamp should be updated
		Assertions.assertEquals(retrievedInputsAt, devSparql.getTimestamp(derivation));
		// new instance set 2 should not be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assertions.assertTrue(!checkInstanceHasRdfType(instance));
		});

		// case 2: as now the timestamp of this derivation is up-to-date, nothing should
		// happen when reconnectNewDerivedIRIs again with a new sets of instances
		triplesChanged = devSparql.reconnectSyncDerivation(derivation, newInstanceMap2, newTriples2, retrievedInputsAt);
		Assertions.assertTrue(!triplesChanged);
		// repeat all checks after case 1
		// all old outputs should be deleted
		for (String instance : oldInstances) {
			Assertions.assertTrue(!checkInstanceHasRdfType(instance));
		}
		// new outputs should be connected to this derivation, and its downstreams
		newInstanceMap1.forEach((instance, dds) -> {
			Assertions.assertTrue(checkTripleExist(instance, DerivationSparql.derivednamespace + "belongsTo", derivation));
			Assertions.assertTrue(checkInstanceHasRdfType(instance));
			if (!dds.isEmpty()) {
				dds.stream().forEach(dd -> {
					Assertions.assertTrue(
							checkTripleExist(dd, DerivationSparql.derivednamespace + "isDerivedFrom", instance));
				});
			}
		});
		// timestamp should be updated
		Assertions.assertEquals(retrievedInputsAt, devSparql.getTimestamp(derivation));
		// new instance set 2 should not be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assertions.assertTrue(!checkInstanceHasRdfType(instance));
		});
	}

	private boolean checkInstanceHasRdfType(String instance) {
		return checkPatternExist(String.format("<%s> a ?type.", instance));
	}

	private boolean checkTripleExist(String s, String p, String o) {
		return checkPatternExist(String.format("<%s> <%s> <%s>.", s, p, o));
	}

	private boolean checkPatternExist(String pattern) {
		return storeClient.executeQuery(
				String.format("ASK { %s }", pattern)).getJSONObject(0).getBoolean("ASK");
	}
}
