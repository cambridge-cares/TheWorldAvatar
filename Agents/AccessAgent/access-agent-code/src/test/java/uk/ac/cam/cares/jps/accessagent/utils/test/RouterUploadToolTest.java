package uk.ac.cam.cares.jps.accessagent.utils.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.json.JSONArray;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.accessagent.utils.RouterUploadTool;
import uk.ac.cam.cares.jps.base.query.MockStoreClient;

class RouterUploadToolTest {
	
	final String label1 = "test1";
	final String label2 = "test2";
	final String expectedAskQuery1 = "ASK WHERE{<"
			+RouterUploadTool.ONTOKGROUTER+label1+"> ?p ?o}";
	
	RouterUploadTool uploader = new RouterUploadTool();
	
	@Test
	void testUploadRoutingData() {	
		
		JSONArray jsonArray = new JSONArray(RouterUploadTestHelper.getJsonString(label1,label2));	
		
		MockStoreClient mockStore = new MockStoreClient();
				
		uploader.uploadRoutingData(jsonArray, mockStore);
		
		assertEquals(10,mockStore.getTotalNumberOfTriples());
		
		JSONArray result = mockStore.executeQuery(RouterUploadTestHelper.getTestQuery(label1));
		String expected = RouterUploadTestHelper.getTestQueryExpected(label1);
		assertEquals(expected,result.toString());
		
		result = mockStore.executeQuery(RouterUploadTestHelper.getTestQuery(label2));
		expected = RouterUploadTestHelper.getTestQueryExpected(label2);
		assertEquals(expected,result.toString());
	}
	
	@Test
	void testCreateCheckLabelQuery() {
		String result = uploader.createCheckLabelQuery(label1);		
		assertEquals(RouterUploadTestHelper.removeWhiteSpace(expectedAskQuery1),RouterUploadTestHelper.removeWhiteSpace(result));
	}
	
	@Test
	void testCheckLabelFalse() {
		
		MockStoreClient mockStore = new MockStoreClient();
		boolean result = uploader.checkLabel(mockStore, expectedAskQuery1);
		assertFalse(result);
	}
	
	@Test
	void testCheckLabelTrue() {
		
		MockStoreClient mockStore = new MockStoreClient();
		
		JSONArray jsonArray = new JSONArray(RouterUploadTestHelper.getJsonString(label1,label2));
		uploader.uploadRoutingData(jsonArray, mockStore);
		
		boolean result = uploader.checkLabel(mockStore, expectedAskQuery1);
		assertTrue(result);
	}
	
	@Test
	void testCreateInsertTriplesUpdate() {
		
		String s = "<"+RouterUploadTool.ONTOKGROUTER+label1+">";
		
		String expected = "INSERT DATA{"+s
				+"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>"
				+"\""+RouterUploadTestHelper.getEndpoint(label1)+"\"."
				+s
				+"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint>"
				+"\""+RouterUploadTestHelper.getEndpoint(label1)+"\"."
				+s
				+RouterUploadTool.RDF_TYPE
				+"<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>."
				+s
				+RouterUploadTool.RDF_TYPE
				+"<http://www.w3.org/2002/07/owl#NamedIndividual>."
				+s
				+"<http://www.w3.org/2000/01/rdf-schema#label>"
				+"\""+label1+"\".}";
		
		UpdateBuilder updateBuilder = uploader.createInsertTriplesUpdate(label1,RouterUploadTestHelper.getEndpoint(label1),RouterUploadTestHelper.getEndpoint(label1));
		String result = updateBuilder.buildRequest().toString();
		
		assertEquals(RouterUploadTestHelper.removeWhiteSpace(expected),RouterUploadTestHelper.removeWhiteSpace(result));
	}
}
