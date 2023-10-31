package uk.ac.cam.cares.jps.base.query;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;

class StoreRouterTest {

	static final String TEST_LABEL = "test1";
	static final String TEST_QUERY_ENDPOINT = "http://www.example.com/testQuery";
	static final String TEST_UPDATE_ENDPOINT = "http://www.example.com/testUpdate";

	static int QUERY_INDEX;
	static int UPDATE_INDEX;

	@BeforeAll
	static void setup()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {

		assertNotNull(StoreRouter.getInstance());
		StoreRouter storeRouter = StoreRouter.getInstance();

		assertNotNull(storeRouter.getClass().getDeclaredField("QUERY_INDEX"));
		Field queryIndexField = storeRouter.getClass().getDeclaredField("QUERY_INDEX");
		queryIndexField.setAccessible(true);
		QUERY_INDEX = queryIndexField.getInt(storeRouter);
		assertEquals(0, QUERY_INDEX);

		assertNotNull(storeRouter.getClass().getDeclaredField("UPDATE_INDEX"));
		Field updateIndexField = storeRouter.getClass().getDeclaredField("UPDATE_INDEX");
		updateIndexField.setAccessible(true);
		UPDATE_INDEX = updateIndexField.getInt(storeRouter);
		assertEquals(1, UPDATE_INDEX);
	}

	@Test
	void testCacheInitialisation()
			throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {

		StoreRouter storeRouter = StoreRouter.getInstance();

		assertNotNull(storeRouter.getClass().getSuperclass().getDeclaredField("cache"));
		Field cacheField = storeRouter.getClass().getSuperclass().getDeclaredField("cache");
		cacheField.setAccessible(true);
		@SuppressWarnings("unchecked")
		CacheInterface<String, String> cache = (CacheInterface<String, String>) cacheField.get(storeRouter);
		assertNotNull(cache);
		assertTrue(cache.capacity() > 0);
	}

	@Test
	void testEndpointInitialisation() {
		assertNotNull(StoreRouter.storeRouterEndpoint);
	}

	@Test
	void testGetFromStore() {

		TripleStoreClientInterface mockStore = new mockOntokgrouter()
				.addQueryEndpoint()
				.addUpdateEndpoint()
				.create();

		List<String> result = StoreRouter.getInstance().getFromStore(TEST_LABEL, mockStore);
		assertEquals(TEST_QUERY_ENDPOINT, result.get(QUERY_INDEX));
		assertEquals(TEST_UPDATE_ENDPOINT, result.get(UPDATE_INDEX));
	}

	@Test
	void testGetFromStoreUpdateEndpointOnly() {

		TripleStoreClientInterface mockStore = new mockOntokgrouter()
				.addUpdateEndpoint()
				.create();

		List<String> result = StoreRouter.getInstance().getFromStore(TEST_LABEL, mockStore);
		assertEquals(TEST_UPDATE_ENDPOINT, result.get(UPDATE_INDEX));
		assertNull(result.get(QUERY_INDEX));
	}

	@Test
	void testGetFromStoreQueryEndpointOnly() {

		TripleStoreClientInterface mockStore = new mockOntokgrouter()
				.addQueryEndpoint()
				.create();

		List<String> result = StoreRouter.getInstance().getFromStore(TEST_LABEL, mockStore);
		assertEquals(TEST_QUERY_ENDPOINT, result.get(QUERY_INDEX));
		assertNull(result.get(UPDATE_INDEX));
	}

	@Test
	void testGetFromStoreNoEndpoints() {

		TripleStoreClientInterface mockStore = new mockOntokgrouter().create();

		List<String> result = StoreRouter.getInstance().getFromStore(TEST_LABEL, mockStore);
		assertNull(result.get(QUERY_INDEX));
		assertNull(result.get(UPDATE_INDEX));
	}

	@Test
	void testGet() {

		List<String> expected = new ArrayList<String>();
		expected.add(QUERY_INDEX, TEST_QUERY_ENDPOINT);
		expected.add(UPDATE_INDEX, TEST_UPDATE_ENDPOINT);

		TripleStoreClientInterface mockStore = new mockOntokgrouter()
				.addQueryEndpoint()
				.addUpdateEndpoint()
				.create();

		StoreRouter storeRouter = StoreRouter.getInstance();

		// Use mocked StoreClient as triple store
		StoreRouter spyStoreRouter = Mockito.spy(storeRouter);
		Mockito.doReturn(mockStore).when(spyStoreRouter).getRouterStoreClient();

		// Not in cache
		assertEquals(expected, spyStoreRouter.get(TEST_LABEL));
		Mockito.verify(spyStoreRouter, Mockito.times(1)).getRouterStoreClient();
		Mockito.verify(spyStoreRouter, Mockito.times(1)).getFromStore(TEST_LABEL, mockStore);

		// In cache so getFromStore and getRouterStoreClient are not called again
		assertEquals(expected, spyStoreRouter.get(TEST_LABEL));
		Mockito.verify(spyStoreRouter, Mockito.times(1)).getRouterStoreClient();
		Mockito.verify(spyStoreRouter, Mockito.times(1)).getFromStore(TEST_LABEL, mockStore);

		// Label does not exist
		String label = "labeldoesnotexist";
		assertNull(spyStoreRouter.get(label));
		Mockito.verify(spyStoreRouter, Mockito.times(2)).getRouterStoreClient();
		// only called once with these arguments
		Mockito.verify(spyStoreRouter, Mockito.times(1)).getFromStore(label, mockStore);
	}

	@Test
	void testIsFileBasedTargetResourceID() {
		// Test different extensions
		assertTrue(StoreRouter.isFileBasedTargetResourceID("kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertTrue(StoreRouter.isFileBasedTargetResourceID("kb/sgp/singapore/SGTemperatureSensor-001.rdf"));
		assertTrue(StoreRouter.isFileBasedTargetResourceID("kb/sgp/singapore/SGTemperatureSensor-001.nt"));

		// Test IRI
		assertTrue(StoreRouter
				.isFileBasedTargetResourceID("http://theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl"));

		// Test not file based resources
		assertFalse(StoreRouter.isFileBasedTargetResourceID("http://theworldavatar.com/kb/ontokin"));
		assertFalse(StoreRouter.isFileBasedTargetResourceID("ontokin"));

		// Invalid file paths
		// These are legal file paths in Linux
		// assertFalse(StoreRouter.isFileBasedTargetResourceID("kb/sgp:singapore/SGTemperatureSensor-001.owl"));
		// assertFalse(StoreRouter.isFileBasedTargetResourceID("kb/sgp?singapore/SGTemperatureSensor-001.owl"));
		// assertFalse(StoreRouter.isFileBasedTargetResourceID("kb/sgp*singapore/SGTemperatureSensor-001.owl"));
	}

	@Test
	void testGetLabelFromTargetResourceID() {
		assertEquals("ontokin", StoreRouter.getLabelFromTargetResourceID("http://theworldavatar.com/kb/ontokin"));
		assertEquals("ontokin", StoreRouter.getLabelFromTargetResourceID("http:///kb/ontokin"));
		assertEquals("ontokin", StoreRouter.getLabelFromTargetResourceID("http:/kb/ontokin"));
		assertEquals("ontokin", StoreRouter.getLabelFromTargetResourceID("ontokin"));
	}

	@Test
	void testGetPathComponent() {
		// test getPath from file based target resource ID
		assertEquals("kb/sgp/singapore/SGTemperatureSensor-001.owl",
				StoreRouter.getPathComponent("kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertEquals("/kb/sgp/singapore/SGTemperatureSensor-001.owl",
				StoreRouter.getPathComponent("http://theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertEquals("/kb/sgp/singapore/SGTemperatureSensor-001.owl",
				StoreRouter.getPathComponent("http:///kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertEquals("/kb/sgp/singapore/SGTemperatureSensor-001.owl",
				StoreRouter.getPathComponent("http:/kb/sgp/singapore/SGTemperatureSensor-001.owl"));

		// test getPath with tomcat root path with file scheme
		assertEquals("/C:/TOMCAT/webapps/ROOT", StoreRouter.getPathComponent("file:///C:/TOMCAT/webapps/ROOT"));
	}

	@Test
	void testJoinPaths() {
		String path1 = "test/path/1";
		String path2 = "test/path/2";
		String expected = path1 + "/" + path2;
		assertEquals(expected, StoreRouter.joinPaths(path1, path2));
		assertEquals(expected, StoreRouter.joinPaths(path1, "/" + path2));
	}

	@Test
	void testISRemoteTargetResourceID() {
		assertTrue(StoreRouter.isRemoteTargetResourceID("http://theworldavatar.com/kb/ontokin"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("ontokin"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("ontokin123"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("123"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("citieskg-berlin"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("kb_ontokin"));
		assertFalse(StoreRouter.isRemoteTargetResourceID("test/ontokin"));
	}

	@Test
	public void testGetRouterStoreClient() {

		StoreRouter storeRouter = StoreRouter.getInstance();

		TripleStoreClientInterface storeClient = storeRouter.getRouterStoreClient();

		assertNotNull(storeClient);
		assertEquals(StoreRouter.storeRouterEndpoint, storeClient.getQueryEndpoint());
	}

	///////////////////////////////////////////
	// Mock ontokgrouter triple store

	private class mockOntokgrouter {

		private MockStoreClient mockStore;

		// construct mock store with no query no update endpoint
		public mockOntokgrouter() {
			mockStore = createMockStoreNoQueryNoUpdate();
		}

		// return created mock ontokgrouter storeclient
		public MockStoreClient create() {
			return mockStore;
		}

		// add query endpoint
		public mockOntokgrouter addQueryEndpoint() {
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/" + TEST_LABEL + ">",
					"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>",
					TEST_QUERY_ENDPOINT);
			return this;
		}

		// add update endpoint
		public mockOntokgrouter addUpdateEndpoint() {
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/" + TEST_LABEL + ">",
					"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint>",
					TEST_UPDATE_ENDPOINT);
			return this;
		}

		private MockStoreClient createMockStoreNoQueryNoUpdate() {

			MockStoreClient mockStore = new MockStoreClient();
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/" + TEST_LABEL + ">",
					"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
					"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>");
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/" + TEST_LABEL + ">",
					"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
					"<http://www.w3.org/2002/07/owl#NamedIndividual");
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/" + TEST_LABEL + ">",
					"<http://www.w3.org/2000/01/rdf-schema#label>",
					TEST_LABEL);

			// Pad store with other data
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/test2>",
					"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>",
					"http://www.example.com/testQuery2");
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/test2>",
					"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint>",
					"http://www.example.com/testUpdate2");
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/test2>",
					"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
					"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>");
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/test2>",
					"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
					"<http://www.w3.org/2002/07/owl#NamedIndividual");
			mockStore.addTriple(
					"<http://www.theworldavatar.com/kb/ontokgrouter/test2>",
					"<http://www.w3.org/2000/01/rdf-schema#label>",
					"test2");

			return mockStore;
		}

	}
}
