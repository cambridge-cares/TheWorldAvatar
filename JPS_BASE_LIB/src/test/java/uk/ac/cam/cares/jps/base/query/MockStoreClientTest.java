package uk.ac.cam.cares.jps.base.query;

import static org.junit.jupiter.api.Assertions.*;

import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

class MockStoreClientTest {
	
	private static String testQuery1 = "SELECT ?o WHERE {<http://www.example.com/test/s1> <http://www.example.com/test/p1> ?o.}";
	private static String expected1 = "[{\"o\":\"http://www.example.com/test/o1\"}]";
	private static String testQuery2 = "SELECT ?o WHERE {<http://www.example.com/test/s2> <http://www.example.com/test/p2> ?o.}";
	private static String expected2 = "[{\"o\":\"http://www.example.com/test/o2\"}]";

	private static String testQuery3 = "SELECT ?o WHERE {<http://www.example.com/test/s3> <http://www.example.com/test/p3> ?o.}";
	private static String expected3 = "[{\"o\":\"testStringLiteral\"}]";
	
	private static String testQueryQuads = "SELECT ?o WHERE {GRAPH <http://www.example.com/test/g> {<http://www.example.com/test/s1> <http://www.example.com/test/p1> ?o.}}";
	private static String expectedQuads = "[{\"o\":\"http://www.example.com/test/o1\"}]";
	
	@Test
	void testAddandClear() {
		MockStoreClient mockStore = new MockStoreClient();
		
		mockStore.addTriple("<http://www.example.com/test/s1>", "<http://www.example.com/test/p1>", "<http://www.example.com/test/o1>");
		mockStore.addTriple("<http://www.example.com/test/s2>", "<http://www.example.com/test/p2>", "<http://www.example.com/test/o2>");
		
		String result = mockStore.execute(testQuery1);
		assertEquals(expected1, result);
		
		result = mockStore.execute(testQuery2);
		assertEquals(expected2, result);
		
		mockStore.clear();
		assertTrue(mockStore.isEmpty());
	}
	
	@Test
	void testAddLiterals() {
		MockStoreClient mockStore = new MockStoreClient();
		
		mockStore.addTriple("<http://www.example.com/test/s3>", "<http://www.example.com/test/p3>", "testStringLiteral");
		
		String result = mockStore.execute(testQuery3);
		assertEquals(expected3, result);
	}
	
	@Test
	void testAddQuad() {
		MockStoreClient mockStore = new MockStoreClient();
		
		mockStore.addQuad("<http://www.example.com/test/g>", "<http://www.example.com/test/s1>", "<http://www.example.com/test/p1>", "<http://www.example.com/test/o1>");
		
		String result = mockStore.execute(testQueryQuads);
		assertEquals(expectedQuads, result);
	}
	
	@Test
	public void testLoad() throws URISyntaxException {
		
		// Test rdf file
		Path testResourcePath = Paths.get(this.getClass().getResource("/KBClientTest/testMock.nt").toURI());		

		MockStoreClient mockStore = new MockStoreClient();
		
		mockStore.load(testResourcePath.toString());
		String result = mockStore.execute(testQuery1);
		assertEquals(expected1, result);
		
		result = mockStore.execute(testQuery2);
		assertEquals(expected2, result);
	}

}
