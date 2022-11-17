package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.update.UpdateRequest;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.MockStoreClient;
import uk.ac.cam.cares.jps.base.tools.SingleStepCloningTool;

class SingleStepCloningToolTest {

	SingleStepCloningTool cloningTool = new SingleStepCloningTool();
	String graph = "http://www.example.com/test";
	
	
	@Test
	void testClone() {

		MockStoreClient sourceStoreClient = new MockStoreClient();
		sourceStoreClient.addTriple("<s>", "<p>", "<o>");
		MockStoreClient targetStoreClient = new MockStoreClient();
						
		cloningTool.clone(sourceStoreClient, targetStoreClient);
		
		WhereBuilder where = new WhereBuilder().addWhere("<s>", "<p>", "<o>");
		assertTrue(CloningToolTestHelper.checkSingleTriple(targetStoreClient,where));
	}

	@Test
	void testCloneNamed() {
		
		MockStoreClient sourceStoreClient = new MockStoreClient();
		sourceStoreClient.addQuad("<"+graph+">", "<s>", "<p>", "<o>");
		MockStoreClient targetStoreClient = new MockStoreClient();
		
		cloningTool.clone(sourceStoreClient, graph, targetStoreClient, graph);
		
		WhereBuilder where = new WhereBuilder().addGraph("<"+graph+">", "<s>", "<p>", "<o>");
		assertTrue(CloningToolTestHelper.checkSingleTriple(targetStoreClient,where));
	}
	
	@Test
	void testCloneNamedToNull() {
		
		MockStoreClient sourceStoreClient = new MockStoreClient();
		sourceStoreClient.addQuad("<"+graph+">", "<s>", "<p>", "<o>");
		MockStoreClient targetStoreClient = new MockStoreClient();
		
		cloningTool.clone(sourceStoreClient, graph, targetStoreClient, null);
		
		WhereBuilder where = new WhereBuilder().addWhere("<s>", "<p>", "<o>");
		assertTrue(CloningToolTestHelper.checkSingleTriple(targetStoreClient,where));
	}
	
	@Test
	void testCloneNullToNamed() {
		
		MockStoreClient sourceStoreClient = new MockStoreClient();
		sourceStoreClient.addTriple("<s>", "<p>", "<o>");
		MockStoreClient targetStoreClient = new MockStoreClient();
		
		cloningTool.clone(sourceStoreClient, null, targetStoreClient, graph);
		
		WhereBuilder where = new WhereBuilder().addGraph("<"+graph+">", "<s>", "<p>", "<o>");
		assertTrue(CloningToolTestHelper.checkSingleTriple(targetStoreClient,where));
	}
	
	@Test
	void testBuildSparqlConstructDefault() {
		
		String expected = "CONSTRUCT {?s ?p ?o.} WHERE {?s ?p ?o}";
		
		Query constructDefault = SingleStepCloningTool.buildSparqlConstruct(null);
		
		assertEquals(CloningToolTestHelper.removeWhiteSpace(expected),CloningToolTestHelper.removeWhiteSpace(constructDefault.toString()));
	}
	
	@Test 
	void testBuildSparqlConstructName() {
		
		String expected = "CONSTRUCT {?s ?p ?o.} WHERE {GRAPH <"+graph+"> {?s ?p ?o}}";
		
		Query constructDefault = SingleStepCloningTool.buildSparqlConstruct(graph);
		
		assertEquals(CloningToolTestHelper.removeWhiteSpace(expected),CloningToolTestHelper.removeWhiteSpace(constructDefault.toString()));
	}
	
	@Test 
	void testBuildSparqlUpdateDefault() {

		String expected = "INSERT DATA {<http://www.example.com/s> <http://www.example.com/p> <http://www.example.com/o>.}";
		
		UpdateRequest update = SingleStepCloningTool.buildSparqlUpdate(null, createMockModel());
		
		assertEquals(CloningToolTestHelper.removeWhiteSpace(expected), CloningToolTestHelper.removeWhiteSpace(update.toString()));
	}	
	
	@Test 
	void testBuildSparqlUpdateNamed() {

		String expected = "INSERT DATA { GRAPH <http://www.example.com/test> "
				+ "{ <http://www.example.com/s> <http://www.example.com/p> <http://www.example.com/o>.}}";
		
		UpdateRequest update = SingleStepCloningTool.buildSparqlUpdate(graph, createMockModel());
		
		assertEquals(CloningToolTestHelper.removeWhiteSpace(expected), CloningToolTestHelper.removeWhiteSpace(update.toString()));
	}
	
	Model createMockModel() {
		
		MockStoreClient storeClient = new MockStoreClient();
		storeClient.addTriple("<http://www.example.com/s>", "<http://www.example.com/p>", "<http://www.example.com/o>");
		return storeClient.executeConstruct("CONSTRUCT {?s ?p ?o.} WHERE {?s ?p ?o}");
	}
}
