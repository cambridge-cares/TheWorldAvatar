package uk.ac.cam.cares.jps.base.derivation;

import java.lang.reflect.Field;
import java.net.URI;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.XSD;
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
	public void testSetOldEntitiesMap1()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException,
			IllegalAccessException {
		// this test case tests public void setOldEntitiesMap(JSONObject
		// oldEntitiesRdfType)
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
	public void testSetOldEntitiesMap2()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException,
			IllegalAccessException {
		// this test case tests public void setOldEntitiesMap(Map<String, String>
		// oldEntitiesRdfTypeMap)
		Map<String, String> oldEntitiesRdfType = new HashMap<>();
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
	}

	@Test
	public void testSetOldEntitiesDownstreamDerivationMap1()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// this test case tests public void
		// setOldEntitiesDownstreamDerivationMap(JSONObject
		// oldEntitiesDownstreamDerivation)
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
	public void testSetOldEntitiesDownstreamDerivationMap2()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		// this test case tests public void
		// setOldEntitiesDownstreamDerivationMap(Map<String, List<String>>
		// oldEntitiesDownstreamDerivationMap)
		Map<String, List<String>> oldEntitiesDownstreamDerivation = new HashMap<>();
		oldEntitiesDownstreamDerivation.put(instance1, Arrays.asList(derivation1_1, derivation1_2));
		oldEntitiesDownstreamDerivation.put(instance2, Arrays.asList(derivation2_1, derivation2_2, derivation2_3));
		oldEntitiesDownstreamDerivation.put(instance3, Arrays.asList(derivation3_1));
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
	}

	@Test
	public void testCreateNewEntity_CreateNewEntityWithBaseUrl_GetNewDerivedIRI_GetNewEntitiesJsonMap()
			throws Exception {
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

		// also add triples with createNewEntityWithBaseUrl
		String baseUrl = "http://" + UUID.randomUUID().toString();
		String rdfTypeClz = UUID.randomUUID().toString();
		String prefix1 = "http://prefix1/"; // try ends with "/"
		String prefix2 = "http://prefix2#"; // try ends with "#"
		String iri_new_1_1 = devOutputs.createNewEntityWithBaseUrl(baseUrl, prefix1 + rdfTypeClz);
		String iri_new_1_2 = devOutputs.createNewEntityWithBaseUrl(baseUrl, prefix1 + rdfTypeClz);
		String iri_new_2_1 = devOutputs.createNewEntityWithBaseUrl(baseUrl, prefix2 + rdfTypeClz);
		String iri_new_2_2 = devOutputs.createNewEntityWithBaseUrl(baseUrl, prefix2 + rdfTypeClz);
		String iri_new_2_3 = devOutputs.createNewEntityWithBaseUrl(baseUrl, prefix2 + rdfTypeClz);
		// should throw an error if the provided rdf:type ends with "/" and nothing will
		// be added
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devOutputs.createNewEntityWithBaseUrl(baseUrl, prefix1 + rdfTypeClz + "/"));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.INVALID_IRI_FOR_GET_CLASS_NAME_ERROR));
		// same error should be thrown for rdf:type ending with "#"
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devOutputs.createNewEntityWithBaseUrl(baseUrl, prefix1 + rdfTypeClz + "#"));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.INVALID_IRI_FOR_GET_CLASS_NAME_ERROR));

		Map<String, List<String>> entities = (Map<String, List<String>>) newentities.get(devOutputs);
		List<TriplePattern> triples = (List<TriplePattern>) outputs.get(devOutputs);

		// the retrieved values must be the same as the ones been added
		// there are 5 rdf:type
		Assert.assertEquals(5, entities.size());
		Assert.assertTrue(equalLists(Arrays.asList(iri1_1, iri1_2), entities.get(rdfType1)));
		Assert.assertTrue(equalLists(Arrays.asList(iri2_1, iri2_2, iri2_3), entities.get(rdfType2)));
		Assert.assertTrue(equalLists(Arrays.asList(iri3_1), entities.get(rdfType3)));
		Assert.assertTrue(equalLists(Arrays.asList(iri_new_1_1, iri_new_1_2), entities.get(prefix1 + rdfTypeClz)));
		Assert.assertTrue(
				equalLists(Arrays.asList(iri_new_2_1, iri_new_2_2, iri_new_2_3), entities.get(prefix2 + rdfTypeClz)));

		// there are 11 triples in total
		Assert.assertEquals(11, triples.size());
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
		Assert.assertEquals(formulateTripleString(iri_new_1_1, RDF.TYPE.toString(), prefix1 + rdfTypeClz),
				triples.get(6).getQueryString());
		Assert.assertEquals(formulateTripleString(iri_new_1_2, RDF.TYPE.toString(), prefix1 + rdfTypeClz),
				triples.get(7).getQueryString());
		Assert.assertEquals(formulateTripleString(iri_new_2_1, RDF.TYPE.toString(), prefix2 + rdfTypeClz),
				triples.get(8).getQueryString());
		Assert.assertEquals(formulateTripleString(iri_new_2_2, RDF.TYPE.toString(), prefix2 + rdfTypeClz),
				triples.get(9).getQueryString());
		Assert.assertEquals(formulateTripleString(iri_new_2_3, RDF.TYPE.toString(), prefix2 + rdfTypeClz),
				triples.get(10).getQueryString());

		// test getNewDerivedIRI
		List<String> allInstances = Arrays.asList(iri1_1, iri1_2, iri2_1, iri2_2, iri2_3, iri3_1, iri_new_1_1,
				iri_new_1_2, iri_new_2_1, iri_new_2_2, iri_new_2_3);
		Assert.assertTrue(equalLists(allInstances, devOutputs.getNewDerivedIRI()));

		Map<String, String> iriRdfTypeMap = new HashMap<>();
		iriRdfTypeMap.put(iri1_1, rdfType1);
		iriRdfTypeMap.put(iri1_2, rdfType1);
		iriRdfTypeMap.put(iri2_1, rdfType2);
		iriRdfTypeMap.put(iri2_2, rdfType2);
		iriRdfTypeMap.put(iri2_3, rdfType2);
		iriRdfTypeMap.put(iri3_1, rdfType3);
		iriRdfTypeMap.put(iri_new_1_1, prefix1 + rdfTypeClz);
		iriRdfTypeMap.put(iri_new_1_2, prefix1 + rdfTypeClz);
		iriRdfTypeMap.put(iri_new_2_1, prefix2 + rdfTypeClz);
		iriRdfTypeMap.put(iri_new_2_2, prefix2 + rdfTypeClz);
		iriRdfTypeMap.put(iri_new_2_3, prefix2 + rdfTypeClz);

		// test getNewEntitiesJsonMap
		JSONObject newEntitiesJson = devOutputs.getNewEntitiesJsonMap();
		allInstances.stream()
				.forEach(iri -> Assert.assertEquals(iriRdfTypeMap.get(iri), newEntitiesJson.getString(iri)));
	}

	@Test
	public void testGetAddTripleAndLiteral()
			throws Exception {
		DerivationOutputs devOutputs = new DerivationOutputs();
		Field outputs = devOutputs.getClass().getDeclaredField("outputTriples");
		outputs.setAccessible(true);

		// prepare the triples
		// IRI referent as object
		String s0 = "http://" + UUID.randomUUID().toString();
		String p0 = "http://" + UUID.randomUUID().toString();
		String o0 = UUID.randomUUID().toString();

		String s1 = "http://" + UUID.randomUUID().toString();
		String p1 = "http://" + UUID.randomUUID().toString();
		String o1 = "http://" + UUID.randomUUID().toString();

		String s2 = "http://" + UUID.randomUUID().toString();
		String p2 = "http://" + UUID.randomUUID().toString();
		String o2 = "ftp://" + UUID.randomUUID().toString();

		String s3 = "http://" + UUID.randomUUID().toString();
		String p3 = "http://" + UUID.randomUUID().toString();
		String o3 = "file://" + UUID.randomUUID().toString();

		// literal value as object
		String s4 = "http://" + UUID.randomUUID().toString();
		String p4 = "http://" + UUID.randomUUID().toString();
		String o4 = UUID.randomUUID().toString();

		String s5 = "http://" + UUID.randomUUID().toString();
		String p5 = "http://" + UUID.randomUUID().toString();
		long o5 = Instant.now().getEpochSecond();

		String s6 = "http://" + UUID.randomUUID().toString();
		String p6 = "http://" + UUID.randomUUID().toString();
		int o6 = 0;

		String s7 = "http://" + UUID.randomUUID().toString();
		String p7 = "http://" + UUID.randomUUID().toString();
		boolean o7 = true;

		String s8 = "http://" + UUID.randomUUID().toString();
		String p8 = "http://" + UUID.randomUUID().toString();
		boolean o8 = false;

		String s9 = "http://" + UUID.randomUUID().toString();
		String p9 = "http://" + UUID.randomUUID().toString();
		String o9 = "48.13188#11.54965#1379714400";
		String dataType = "http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon-time";

		String s10 = "http://" + UUID.randomUUID().toString();
		String p10 = "http://" + UUID.randomUUID().toString();
		String o10 = "this is a string with space"; // test string with space are correctly handled when adding as literal

		// add literal numbers, including special cases
		String s11 = "http://" + UUID.randomUUID().toString();
		String p11 = "http://" + UUID.randomUUID().toString();
		Double o11 = Double.NaN; // test NaN

		String s12 = "http://" + UUID.randomUUID().toString();
		String p12 = "http://" + UUID.randomUUID().toString();
		Double o12 = Double.POSITIVE_INFINITY; // test positive infinity INF

		String s13 = "http://" + UUID.randomUUID().toString();
		String p13 = "http://" + UUID.randomUUID().toString();
		Double o13 = Double.NEGATIVE_INFINITY; // test negative infinity -INF

		String s14 = "http://" + UUID.randomUUID().toString();
		String p14 = "http://" + UUID.randomUUID().toString();
		Double o14 = 2.3; // test a normal number

		// addTriple(List<TriplePattern>) is tested automatically
		devOutputs.addTriple(Rdf.iri(s0).has(Rdf.iri(p0), Rdf.iri(o0)));
		devOutputs.addTriple("<" + s1, p1 + ">", o1);
		devOutputs.addTriple(s2, "<" + p2, o2 + ">");
		devOutputs.addTriple(s3, "<" + p3, o3 + ">");

		// addLiteral
		devOutputs.addLiteral(s4, "<" + p4 + ">", o4);
		devOutputs.addLiteral(s5 + ">", p5, o5);
		devOutputs.addLiteral("<" + s6 + ">", "<" + p6 + ">", o6);
		devOutputs.addLiteral("<" + s7 + ">", "<" + p7 + ">", o7);
		devOutputs.addLiteral(s8 + ">", "<" + p8 + ">", o8);
		devOutputs.addLiteral(s9, p9, o9, dataType);
		devOutputs.addLiteral(s10, p10, o10);
		devOutputs.addLiteral(s11, p11, o11);
		devOutputs.addLiteral(s12, p12, o12);
		devOutputs.addLiteral(s13, p13, o13);
		devOutputs.addLiteral(s14, p14, o14);

		List<TriplePattern> triples = (List<TriplePattern>) outputs.get(devOutputs);
		// the amount of triples added must be correct, also the content must be correct
		Assert.assertEquals(15, triples.size());
		Assert.assertEquals(formulateTripleString(s0, p0, o0), triples.get(0).getQueryString());
		Assert.assertEquals(formulateTripleString(s1, p1, o1), triples.get(1).getQueryString());
		Assert.assertEquals(formulateTripleString(s2, p2, o2), triples.get(2).getQueryString());
		Assert.assertEquals(formulateTripleString(s3, p3, o3), triples.get(3).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s4, p4, o4), triples.get(4).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s5, p5, o5), triples.get(5).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s6, p6, o6), triples.get(6).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s7, p7, o7), triples.get(7).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s8, p8, o8), triples.get(8).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s9, p9, o9, dataType), triples.get(9).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s10, p10, o10), triples.get(10).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s11, p11, o11), triples.get(11).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s12, p12, o12), triples.get(12).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s13, p13, o13), triples.get(13).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s14, p14, o14), triples.get(14).getQueryString());

		// test the getter
		List<TriplePattern> triplesFromGetter = devOutputs.getOutputTriples();
		Assert.assertEquals(15, triplesFromGetter.size());
		Assert.assertEquals(formulateTripleString(s0, p0, o0), triplesFromGetter.get(0).getQueryString());
		Assert.assertEquals(formulateTripleString(s1, p1, o1), triplesFromGetter.get(1).getQueryString());
		Assert.assertEquals(formulateTripleString(s2, p2, o2), triplesFromGetter.get(2).getQueryString());
		Assert.assertEquals(formulateTripleString(s3, p3, o3), triplesFromGetter.get(3).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s4, p4, o4), triplesFromGetter.get(4).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s5, p5, o5), triplesFromGetter.get(5).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s6, p6, o6), triplesFromGetter.get(6).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s7, p7, o7), triplesFromGetter.get(7).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s8, p8, o8), triplesFromGetter.get(8).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s9, p9, o9, dataType), triplesFromGetter.get(9).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s10, p10, o10), triplesFromGetter.get(10).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s11, p11, o11), triplesFromGetter.get(11).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s12, p12, o12), triplesFromGetter.get(12).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s13, p13, o13), triplesFromGetter.get(13).getQueryString());
		Assert.assertEquals(formulateLiteralTripleString(s14, p14, o14), triplesFromGetter.get(14).getQueryString());

		// now we add triples/literals that with invalid IRIs
		// should throw an error
		String sInvalidIRI = "http://" + "this string contains space therefore invalid" + UUID.randomUUID().toString();
		String pInvalidIRI = "http://" + "this string contains space therefore invalid" + UUID.randomUUID().toString();
		String oInvalidIRI = "http://" + "this string contains space therefore invalid" + UUID.randomUUID().toString();
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devOutputs.addTriple(sInvalidIRI, pInvalidIRI, oInvalidIRI));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.INVALID_IRI_FOR_ADDING_TRIPLE));
		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> devOutputs.addLiteral(sInvalidIRI, pInvalidIRI, oInvalidIRI));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.INVALID_IRI_FOR_ADDING_LITERAL));

		// if the subject and preficate are valid, then it should be able to be added as literal even the object is not valid IRI
		devOutputs.addLiteral(sInvalidIRI.replace(" ", ""), pInvalidIRI.replace(" ", ""), oInvalidIRI);
	}

	@Test
	public void testCheckIfValidIri() {
		String http = "http://" + UUID.randomUUID().toString();
		String https = "https://" + UUID.randomUUID().toString();
		String p = UUID.randomUUID().toString();
		String cmt = "comment";
		DerivationOutputs outputs = new DerivationOutputs();

		// should execute without error
		outputs.checkIfValidIri(Arrays.asList(http, https, "<" + http + ">", "<" + https + ">"));

		// should throw error if not valid IRI
		JPSRuntimeException e = Assert.assertThrows(JPSRuntimeException.class,
				() -> outputs.checkIfValidIri(Arrays.asList(http, p)));
		Assert.assertTrue(e.getMessage()
				.contains(DerivationOutputs.INVALID_IRI_ERROR));

		e = Assert.assertThrows(JPSRuntimeException.class,
				() -> outputs.checkIfValidIri(Arrays.asList(http, cmt)));
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

	public String formulateTripleString(String s, String p, String o) {
		return "<" + s + "> <" + p + "> <" + o + "> .";
	}

	public String formulateLiteralTripleString(String s, String p, Object o) {
		if (o instanceof String) {
			return "<" + s + "> <" + p + "> \"" + o + "\" .";
		} else if (o instanceof Number) {
			if (o instanceof Double) {
				if (((Double) o).isInfinite()) {
					if (((Double) o) > 0) {
						return "<" + s + "> <" + p + "> \"Infinity\"^^<" + XSD.DOUBLE.toString() + "> .";
					} else {
						return "<" + s + "> <" + p + "> \"-Infinity\"^^<" + XSD.DOUBLE.toString() + "> .";
					}
				} else if (((Double) o).isNaN()) {
					return "<" + s + "> <" + p + "> \"NaN\"^^<" + XSD.DOUBLE.toString() + "> .";
				} else {
					return "<" + s + "> <" + p + "> " + o + " .";
				}
			} else {
				return "<" + s + "> <" + p + "> " + o + " .";
			}
		} else if (o instanceof Boolean) {
			return "<" + s + "> <" + p + "> " + String.valueOf(o) + " .";
		} else {
			return null;
		}
	}

	public String formulateLiteralTripleString(String s, String p, String o, String dataType) {
		return "<" + s + "> <" + p + "> \"" + o + "\"^^<" + dataType + "> .";
	}
}
