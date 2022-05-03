package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DerivationOutputsTest {
	private String class1 = "http://class1";
	private String class2 = "http://class2";
	private String class3 = "http://class3";
	private String class4 = "http://class4";
	private String instance1 = "http://instance1";
	private String instance2 = "http://instance2";
	private String instance3 = "http://instance3";
	private String instance4 = "http://instance4";
	private String derivation1_1 = "http://derivation1_1";
	private String derivation1_2 = "http://derivation1_2";
	private String derivation2_1 = "http://derivation2_1";
	private String derivation2_2 = "http://derivation2_2";
	private String derivation2_3 = "http://derivation2_3";
	private String derivation3_1 = "http://derivation3_1";

	@Test
	public void testConstructor()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// initialise with constructor DerivationOutputs()
		DerivationOutputs devOutputs = new DerivationOutputs();
		// Retrieve the value of the private fields of the client
		Field _oldEntitiesMap = devOutputs.getClass().getDeclaredField("oldEntitiesMap");
		_oldEntitiesMap.setAccessible(true);
		Map<String, List<String>> oldEntitiesMap = (Map<String, List<String>>) _oldEntitiesMap.get(devOutputs);

		Field _oldEntitiesDownstreamDerivationMap = devOutputs.getClass()
				.getDeclaredField("oldEntitiesDownstreamDerivationMap");
		_oldEntitiesDownstreamDerivationMap.setAccessible(true);
		Map<String, List<String>> oldEntitiesDownstreamDerivationMap = (Map<String, List<String>>) _oldEntitiesDownstreamDerivationMap
				.get(devOutputs);

		Field _newEntitiesMap = devOutputs.getClass().getDeclaredField("newEntitiesMap");
		_newEntitiesMap.setAccessible(true);
		Map<String, List<String>> newEntitiesMap = (Map<String, List<String>>) _newEntitiesMap.get(devOutputs);

		Field _outputTriples = devOutputs.getClass().getDeclaredField("outputTriples");
		_outputTriples.setAccessible(true);
		List<TriplePattern> outputTriples = (List<TriplePattern>) _outputTriples.get(devOutputs);

		Field _thisDerivation = devOutputs.getClass().getDeclaredField("thisDerivation");
		_thisDerivation.setAccessible(true);
		String thisDerivation = (String) _thisDerivation.get(devOutputs);

		Field _retrievedInputsAt = devOutputs.getClass().getDeclaredField("retrievedInputsAt");
		_retrievedInputsAt.setAccessible(true);
		long retrievedInputsAt = (long) _retrievedInputsAt.get(devOutputs);

		// the retrieved value should be either empty of null
		Assert.assertTrue(oldEntitiesMap.isEmpty());
		Assert.assertTrue(oldEntitiesDownstreamDerivationMap.isEmpty());
		Assert.assertTrue(newEntitiesMap.isEmpty());
		Assert.assertTrue(outputTriples.isEmpty());
		Assert.assertTrue(thisDerivation.isEmpty());
		Assert.assertEquals(0, retrievedInputsAt);
	}

	@Test
	public void testGetSetThisDerivation()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		String derivation = "http://" + UUID.randomUUID().toString();
		DerivationOutputs devOutputs = new DerivationOutputs();
		devOutputs.setThisDerivation(derivation);

		// Retrieve the value of the private fields of the client
		Field _thisDerivation = devOutputs.getClass().getDeclaredField("thisDerivation");
		_thisDerivation.setAccessible(true);
		String thisDerivation = (String) _thisDerivation.get(devOutputs);

		// the retrieved value should be equal to the one used for setter
		Assert.assertEquals(derivation, thisDerivation);
		// also test the getter
		Assert.assertEquals(derivation, devOutputs.getThisDerivation());
	}

	@Test
	public void testGetSetRetrievedInputsAt()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		long timestamp = Instant.now().getEpochSecond();
		DerivationOutputs devOutputs = new DerivationOutputs();
		devOutputs.setRetrievedInputsAt(timestamp);

		// Retrieve the value of the private fields of the client
		Field _retrievedInputsAt = devOutputs.getClass().getDeclaredField("retrievedInputsAt");
		_retrievedInputsAt.setAccessible(true);
		long retrievedInputsAt = (long) _retrievedInputsAt.get(devOutputs);

		// the retrieved value should be equal to the one used for setter
		Assert.assertEquals(timestamp, retrievedInputsAt);
		// also test the getter
		Assert.assertEquals(timestamp, devOutputs.getRetrievedInputsAt());
	}

	@Test
	public void testSetOldEntitiesMap()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException,
			IllegalAccessException {
		JSONObject oldEntitiesRdfType = new JSONObject();
		oldEntitiesRdfType.put(instance1, class1);
		oldEntitiesRdfType.put(instance2, class2);
		oldEntitiesRdfType.put(instance3, class3);
		DerivationOutputs devOutputs = new DerivationOutputs();
		devOutputs.setOldEntitiesMap(oldEntitiesRdfType);
		// Retrieve the value of the private field 'outputs' of the client
		Field outputs = devOutputs.getClass().getDeclaredField("oldEntitiesMap");
		outputs.setAccessible(true);
		Map<String, String> map = (Map<String, String>) outputs.get(devOutputs);
		// the retrieved value should correctly processed the JSONObject
		Assert.assertEquals(class1, map.get(instance1));
		Assert.assertEquals(class2, map.get(instance2));
		Assert.assertEquals(class3, map.get(instance3));

		// should fail if given wrong JSON structure
		oldEntitiesRdfType.put("http://key", new JSONObject().put("http://key2", "http://value"));
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devOutputs.setOldEntitiesMap(oldEntitiesRdfType));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.OLD_ENTITIES_MAP_ERROR + oldEntitiesRdfType.toString()));
	}

	@Test
	public void testSetOldEntitiesDownstreamDerivationMap()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		JSONObject oldEntitiesDownstreamDerivation = new JSONObject();
		oldEntitiesDownstreamDerivation.put(instance1, new JSONArray(Arrays.asList(derivation1_1, derivation1_2)));
		oldEntitiesDownstreamDerivation.put(instance2,
				new JSONArray(Arrays.asList(derivation2_1, derivation2_2, derivation2_3)));
		oldEntitiesDownstreamDerivation.put(instance3, new JSONArray(Arrays.asList(derivation3_1)));
		DerivationOutputs devOutputs = new DerivationOutputs();
		devOutputs.setOldEntitiesDownstreamDerivationMap(oldEntitiesDownstreamDerivation);
		// Retrieve the value of the private field 'outputs' of the client
		Field outputs = devOutputs.getClass().getDeclaredField("oldEntitiesDownstreamDerivationMap");
		outputs.setAccessible(true);
		Map<String, List<String>> map = (Map<String, List<String>>) outputs.get(devOutputs);
		// the retrieved value should correctly processed the JSONObject
		Assert.assertTrue(equalLists(Arrays.asList(derivation1_1, derivation1_2), map.get(instance1)));
		Assert.assertTrue(equalLists(Arrays.asList(derivation2_1, derivation2_2, derivation2_3), map.get(instance2)));
		Assert.assertTrue(equalLists(Arrays.asList(derivation3_1), map.get(instance3)));

		// should fail if given wrong JSON structure
		oldEntitiesDownstreamDerivation.put("http://key", new JSONObject().put("http://key2", "http://value"));
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devOutputs.setOldEntitiesDownstreamDerivationMap(oldEntitiesDownstreamDerivation));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.OLD_ENTITIES_DOWNSTREAM_DERIVATION_MAP_ERROR
						+ oldEntitiesDownstreamDerivation.toString()));
	}

	@Test
	public void testCreateNewEntity_GetNewDerivedIRI()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		DerivationOutputs devOutputs = new DerivationOutputs();
		Field newentities = devOutputs.getClass().getDeclaredField("newEntitiesMap");
		newentities.setAccessible(true);
		Field outputs = devOutputs.getClass().getDeclaredField("outputTriples");
		outputs.setAccessible(true);

		String rdfType1 = "http://" + UUID.randomUUID().toString();
		String rdfType2 = "http://" + UUID.randomUUID().toString();
		String rdfType3 = "http://" + UUID.randomUUID().toString();
		String iri1_1 = "http://" + UUID.randomUUID().toString();
		String iri1_2 = "http://" + UUID.randomUUID().toString();
		String iri2_1 = "http://" + UUID.randomUUID().toString();
		String iri2_2 = "http://" + UUID.randomUUID().toString();
		String iri2_3 = "http://" + UUID.randomUUID().toString();
		String iri3_1 = "http://" + UUID.randomUUID().toString();

		devOutputs.createNewEntity("<" + iri1_1, rdfType1);
		devOutputs.createNewEntity(iri1_2, "<" + rdfType1);
		devOutputs.createNewEntity("<" + iri2_1 + ">", rdfType2);
		devOutputs.createNewEntity(iri2_2, rdfType2 + ">");
		devOutputs.createNewEntity(iri2_3 + ">", rdfType2 + ">");
		devOutputs.createNewEntity("<" + iri3_1 + ">", "<" + rdfType3 + ">");
		devOutputs.createNewEntity(iri3_1, rdfType3); // add duplicate entry on purpose

		Map<String, List<String>> entities = (Map<String, List<String>>) newentities.get(devOutputs);
		List<TriplePattern> triples = (List<TriplePattern>) outputs.get(devOutputs);

		// the retrieved values must be the same as the ones been added
		Assert.assertEquals(3, entities.size());
		Assert.assertTrue(equalLists(Arrays.asList(iri1_1, iri1_2), entities.get(rdfType1)));
		Assert.assertTrue(equalLists(Arrays.asList(iri2_1, iri2_2, iri2_3), entities.get(rdfType2)));
		Assert.assertTrue(equalLists(Arrays.asList(iri3_1), entities.get(rdfType3)));
		Assert.assertEquals(6, triples.size());
		Assert.assertEquals(formulateTripleString(iri1_1, RDF.TYPE.toString(), rdfType1),
				triples.get(0).getQueryString());
		Assert.assertEquals(formulateTripleString(iri1_2, RDF.TYPE.toString(), rdfType1),
				triples.get(1).getQueryString());
		Assert.assertEquals(formulateTripleString(iri2_1, RDF.TYPE.toString(), rdfType2),
				triples.get(2).getQueryString());
		Assert.assertEquals(formulateTripleString(iri2_2, RDF.TYPE.toString(), rdfType2),
				triples.get(3).getQueryString());
		Assert.assertEquals(formulateTripleString(iri2_3, RDF.TYPE.toString(), rdfType2),
				triples.get(4).getQueryString());
		Assert.assertEquals(formulateTripleString(iri3_1, RDF.TYPE.toString(), rdfType3),
				triples.get(5).getQueryString());

		// test getNewDerivedIRI
		Assert.assertTrue(equalLists(Arrays.asList(iri1_1, iri1_2, iri2_1, iri2_2, iri2_3, iri3_1),
				devOutputs.getNewDerivedIRI()));
	}

	@Test
	public void testGetAddTriple()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		DerivationOutputs devOutputs = new DerivationOutputs();
		Field outputs = devOutputs.getClass().getDeclaredField("outputTriples");
		outputs.setAccessible(true);

		// prepare the triples
		String s0 = "http://" + UUID.randomUUID().toString();
		String p0 = "http://" + UUID.randomUUID().toString();
		String o0 = UUID.randomUUID().toString();

		String s1 = "http://" + UUID.randomUUID().toString();
		String p1 = "http://" + UUID.randomUUID().toString();
		String o1 = "http://" + UUID.randomUUID().toString();

		String s2 = "http://" + UUID.randomUUID().toString();
		String p2 = "http://" + UUID.randomUUID().toString();
		String o2 = UUID.randomUUID().toString();

		String s3 = "http://" + UUID.randomUUID().toString();
		String p3 = "http://" + UUID.randomUUID().toString();
		long o3 = Instant.now().getEpochSecond();

		String s4 = "http://" + UUID.randomUUID().toString();
		String p4 = "http://" + UUID.randomUUID().toString();
		int o4 = 0;

		String s5 = "http://" + UUID.randomUUID().toString();
		String p5 = "http://" + UUID.randomUUID().toString();
		boolean o5 = true;

		String s6 = "http://" + UUID.randomUUID().toString();
		String p6 = "http://" + UUID.randomUUID().toString();
		boolean o6 = false;

		String s7 = "http://" + UUID.randomUUID().toString();
		String p7 = "http://" + UUID.randomUUID().toString();
		String o7 = "48.13188#11.54965#1379714400";
		String dataType = "http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon-time";

		// addTriple(List<TriplePattern>) is tested automatically
		devOutputs.addTriple(Rdf.iri(s0).has(Rdf.iri(p0), o0));
		devOutputs.addTriple("<" + s1, p1 + ">", o1);
		devOutputs.addTriple(s2, "<" + p2, o2 + ">");
		devOutputs.addTriple(s3 + ">", p3, o3);
		devOutputs.addTriple("<" + s4 + ">", "<" + p4 + ">", o4);
		devOutputs.addTriple("<" + s5 + ">", "<" + p5 + ">", o5);
		devOutputs.addTriple(s6 + ">", "<" + p6 + ">", o6);
		devOutputs.addTriple(s7, p7, o7, dataType);

		List<TriplePattern> triples = (List<TriplePattern>) outputs.get(devOutputs);
		// the amount of triples added must be correct, also the content must be correct
		Assert.assertEquals(8, triples.size());
		Assert.assertEquals(formulateTripleString(s0, p0, o0), triples.get(0).getQueryString());
		Assert.assertEquals(formulateTripleString(s1, p1, o1), triples.get(1).getQueryString());
		Assert.assertEquals(formulateTripleString(s2, p2, o2), triples.get(2).getQueryString());
		Assert.assertEquals(formulateTripleString(s3, p3, o3), triples.get(3).getQueryString());
		Assert.assertEquals(formulateTripleString(s4, p4, o4), triples.get(4).getQueryString());
		Assert.assertEquals(formulateTripleString(s5, p5, o5), triples.get(5).getQueryString());
		Assert.assertEquals(formulateTripleString(s6, p6, o6), triples.get(6).getQueryString());
		Assert.assertEquals(formulateTripleString(s7, p7, o7, dataType), triples.get(7).getQueryString());

		// test the getter
		List<TriplePattern> triplesFromGetter = devOutputs.getOutputTriples();
		Assert.assertEquals(8, triplesFromGetter.size());
		Assert.assertEquals(formulateTripleString(s0, p0, o0), triplesFromGetter.get(0).getQueryString());
		Assert.assertEquals(formulateTripleString(s1, p1, o1), triplesFromGetter.get(1).getQueryString());
		Assert.assertEquals(formulateTripleString(s2, p2, o2), triplesFromGetter.get(2).getQueryString());
		Assert.assertEquals(formulateTripleString(s3, p3, o3), triplesFromGetter.get(3).getQueryString());
		Assert.assertEquals(formulateTripleString(s4, p4, o4), triplesFromGetter.get(4).getQueryString());
		Assert.assertEquals(formulateTripleString(s5, p5, o5), triplesFromGetter.get(5).getQueryString());
		Assert.assertEquals(formulateTripleString(s6, p6, o6), triplesFromGetter.get(6).getQueryString());
		Assert.assertEquals(formulateTripleString(s7, p7, o7, dataType), triplesFromGetter.get(7).getQueryString());
	}

	@Test
	public void testGetNewEntitiesDownstreamDerivationMap()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		DerivationOutputs devOutputs = new DerivationOutputs();

		// prepare JSONObject
		JSONObject oldEntitiesRdfType = new JSONObject();
		oldEntitiesRdfType.put(instance1, class1);
		oldEntitiesRdfType.put(instance2, class2);
		oldEntitiesRdfType.put(instance3, class3);
		oldEntitiesRdfType.put(instance4, class4);

		JSONObject oldEntitiesDownstreamDerivation = new JSONObject();
		oldEntitiesDownstreamDerivation.put(instance1, new JSONArray(Arrays.asList(derivation1_1, derivation1_2)));
		oldEntitiesDownstreamDerivation.put(instance2,
				new JSONArray(Arrays.asList(derivation2_1, derivation2_2, derivation2_3)));
		oldEntitiesDownstreamDerivation.put(instance3, new JSONArray(Arrays.asList(derivation3_1)));

		// set up the maps
		devOutputs.setOldEntitiesMap(oldEntitiesRdfType);
		devOutputs.setOldEntitiesDownstreamDerivationMap(oldEntitiesDownstreamDerivation);

		// add new entities
		String newEntity1 = "http://" + UUID.randomUUID().toString();
		String newEntity2 = "https://" + UUID.randomUUID().toString(); // test also IRI starts with "https://"
		String newEntity3 = "http://" + UUID.randomUUID().toString();
		String newEntity4 = "https://" + UUID.randomUUID().toString();
		devOutputs.createNewEntity(newEntity1, class1);
		devOutputs.createNewEntity(newEntity2, class2);
		devOutputs.createNewEntity(newEntity3, class3);
		devOutputs.createNewEntity(newEntity4, class4);

		// test if the newEntitiesDownstreamDerivationMap was mapped correctly
		Map<String, List<String>> newEntitiesDownstreamDerivationMap = devOutputs
				.getNewEntitiesDownstreamDerivationMap();
		Assert.assertTrue(equalLists(Arrays.asList(derivation1_1, derivation1_2),
				newEntitiesDownstreamDerivationMap.get(newEntity1)));
		Assert.assertTrue(equalLists(Arrays.asList(derivation2_1, derivation2_2, derivation2_3),
				newEntitiesDownstreamDerivationMap.get(newEntity2)));
		Assert.assertTrue(equalLists(Arrays.asList(derivation3_1),
				newEntitiesDownstreamDerivationMap.get(newEntity3)));
		Assert.assertTrue(newEntitiesDownstreamDerivationMap.get(newEntity4).isEmpty());

		// now we add one more new entity to have multiple matched instances on purpose
		// should throw an error
		String newEntity5 = "http://" + UUID.randomUUID().toString();
		devOutputs.createNewEntity(newEntity5, class3);
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devOutputs.getNewEntitiesDownstreamDerivationMap());
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.OLD_NEW_ENTITIES_MATCHING_ERROR));
	}

	@Test
	public void testCheckIfValidIri() {
		String http = "http://" + UUID.randomUUID().toString();
		String https = "https://" + UUID.randomUUID().toString();
		String p = UUID.randomUUID().toString();
		DerivationOutputs outputs = new DerivationOutputs();

		// should execute without error
		outputs.checkIfValidIri(http, https);
		outputs.checkIfValidIri(https, http);
		outputs.checkIfValidIri("<" + http + ">", "<" + https + ">");

		// should throw error if not valid IRI
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> outputs.checkIfValidIri(http, p));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.INVALID_IRI_ERROR));

		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> outputs.checkIfValidIri(https, p));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.INVALID_IRI_ERROR));
	}

	////////////////////////////////////////////////////////////
	// Below are utility functions to reduce code-duplication //
	////////////////////////////////////////////////////////////

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

	public String formulateTripleString(String s, String p, Object o) {
		if (o instanceof String) {
			if (((String) o).matches(DerivationOutputs.HTTP_HTTPS_PROTOCOL)) {
				return "<" + s + "> <" + p + "> <" + o + "> .";
			} else {
				return "<" + s + "> <" + p + "> \"" + o + "\" .";
			}
		} else if (o instanceof Number) {
			return "<" + s + "> <" + p + "> " + o + " .";
		} else if (o instanceof Boolean) {
			return "<" + s + "> <" + p + "> " + String.valueOf(o) + " .";
		} else {
			return null;
		}
	}

	public String formulateTripleString(String s, String p, String o, String dataType) {
		return "<" + s + "> <" + p + "> \"" + o + "\"^^<" + dataType + "> .";
	}
}
