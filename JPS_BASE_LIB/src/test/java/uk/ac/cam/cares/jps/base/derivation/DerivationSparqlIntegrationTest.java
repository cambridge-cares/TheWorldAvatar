package uk.ac.cam.cares.jps.base.derivation;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.AfterAll;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@Testcontainers
public class DerivationSparqlIntegrationTest {
	static RemoteStoreClient storeClient;
	static DerivationSparql devSparql;
	static String kgUrl;
	static final String agentIRI = "http://agentIRI";
	static final String derivationBaseUrl = "http://derivation/";

	// NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
	// For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
	@Container
	private static GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
			.withExposedPorts(9999); // the port is set as 9999 to match with the value set in the docker image

	@BeforeClass
	public static void initialise()
			throws NoSuchMethodException, SecurityException {
		// create the container in a clean state
		try {
			blazegraph.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("DerivationClientIntegrationTest: Docker container startup failed. Please try running tests again");
		}

		// initialise all variables to be used
		kgUrl = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort() + "/blazegraph/namespace/kb/sparql";
		System.out.println(kgUrl);
		storeClient = new RemoteStoreClient(kgUrl, kgUrl);
		devSparql = new DerivationSparql(storeClient, derivationBaseUrl);
	}

	@AfterAll
	public static void stopContainers() {
		// close containers after all tests
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
	}

	/**
	 * This method serves as integration test for method
	 * DerivationSparql::updateDerivationIfHttpPostAvailable.
	 */
	@Test
	public void testReconnectNewDerivedIRIs() {
		String derivedAgentIRI = "http://" + UUID.randomUUID().toString();
		List<String> inputs = Arrays.asList("http://" + UUID.randomUUID().toString(), "http://" + UUID.randomUUID().toString());
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
		// timestamp for all inputs should already be added automatically when createDerivation

		// test if derivation was created correctly agent
		Assert.assertTrue(checkTripleExist(derivation, DerivationSparql.derivednamespace + "isDerivedUsing", derivedAgentIRI));

		// outputs
		for (String instance : oldInstances) {
			Assert.assertTrue(checkTripleExist(instance, DerivationSparql.derivednamespace + "belongsTo", derivation));
		}
		// inputs
		for (String input : inputs) {
			Assert.assertTrue(checkTripleExist(derivation, DerivationSparql.derivednamespace + "isDerivedFrom", input));
		}
		// new instance set 1 should NOT be created
		newInstanceMap1.keySet().stream().forEach(instance -> {
			Assert.assertTrue(!checkInstanceHasRdfType(instance));
		});
		// new instance set 2 should NOT be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assert.assertTrue(!checkInstanceHasRdfType(instance));
		});

		// case 1: derivation is outdated, now delete and add new instances should work
		long retrievedInputsAt = Instant.now().getEpochSecond();
		boolean triplesChanged = devSparql.reconnectNewDerivedIRIs(newTriples1, newInstanceMap1, derivation, retrievedInputsAt);
		Assert.assertTrue(triplesChanged);
		// all old outputs should be deleted
		for (String instance : oldInstances) {
			Assert.assertTrue(!checkInstanceHasRdfType(instance));
		}
		// new outputs should be connected to this derivation, and its downstreams
		newInstanceMap1.forEach((instance, dds) -> {
			Assert.assertTrue(checkTripleExist(instance, DerivationSparql.derivednamespace + "belongsTo", derivation));
			Assert.assertTrue(checkInstanceHasRdfType(instance));
			if (!dds.isEmpty()) {
				dds.stream().forEach(dd -> {
					Assert.assertTrue(checkTripleExist(dd, DerivationSparql.derivednamespace + "isDerivedFrom", instance));
				});
			}
		});
		// timestamp should be updated
		Assert.assertEquals(retrievedInputsAt, devSparql.getTimestamp(derivation));
		// new instance set 2 should not be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assert.assertTrue(!checkInstanceHasRdfType(instance));
		});

		// case 2: as now the timestamp of this derivation is up-to-date, nothing should
		// happen when reconnectNewDerivedIRIs again with a new sets of instances
		triplesChanged = devSparql.reconnectNewDerivedIRIs(newTriples2, newInstanceMap2, derivation, retrievedInputsAt);
		Assert.assertTrue(!triplesChanged);
		// repeat all checks after case 1
		// all old outputs should be deleted
		for (String instance : oldInstances) {
			Assert.assertTrue(!checkInstanceHasRdfType(instance));
		}
		// new outputs should be connected to this derivation, and its downstreams
		newInstanceMap1.forEach((instance, dds) -> {
			Assert.assertTrue(checkTripleExist(instance, DerivationSparql.derivednamespace + "belongsTo", derivation));
			Assert.assertTrue(checkInstanceHasRdfType(instance));
			if (!dds.isEmpty()) {
				dds.stream().forEach(dd -> {
					Assert.assertTrue(checkTripleExist(dd, DerivationSparql.derivednamespace + "isDerivedFrom", instance));
				});
			}
		});
		// timestamp should be updated
		Assert.assertEquals(retrievedInputsAt, devSparql.getTimestamp(derivation));
		// new instance set 2 should not be created
		newInstanceMap2.keySet().stream().forEach(instance -> {
			Assert.assertTrue(!checkInstanceHasRdfType(instance));
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
