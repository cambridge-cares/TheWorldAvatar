package uk.ac.cam.cares.jps.base.tools.cloning;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.MockStoreClient;

class CloningToolTest {
	
	@Test
	void testConstructor() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {

		CloningTool cloningTool = new CloningTool();
		
		// access private member
		assertNotNull(CloningTool.class.getDeclaredField("stepSize"));
		Field field = CloningTool.class.getDeclaredField("stepSize");
        field.setAccessible(true);
        int defaultStepSize = (int) field.get(cloningTool);
               
        assertNotNull(CloningTool.class.getDeclaredField("defaultOverlapRatio"));
		field = CloningTool.class.getDeclaredField("defaultOverlapRatio");
        field.setAccessible(true);
        double defaultOverlap = (double) field.get(cloningTool);
        
        int overlap = (int) (defaultOverlap*defaultStepSize);
        
        assertNotNull(CloningTool.class.getDeclaredField("overlap"));
		field = CloningTool.class.getDeclaredField("overlap");
        field.setAccessible(true);
        int value = (int) field.get(cloningTool);
        assertEquals(overlap,value);
	}
	
	@Test
	void testConstructorWithStepSizeAndOverlap() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		int stepSize = 2000;
		int overlap = 20;
		CloningTool cloningTool = new CloningTool(stepSize, overlap);
		
		// access private member
		assertNotNull(CloningTool.class.getDeclaredField("stepSize"));
		Field field = CloningTool.class.getDeclaredField("stepSize");
        field.setAccessible(true);
        int value = (int) field.get(cloningTool);
        assertEquals(stepSize,value);
        
        assertNotNull(CloningTool.class.getDeclaredField("overlap"));
		field = CloningTool.class.getDeclaredField("overlap");
        field.setAccessible(true);
        value = (int) field.get(cloningTool);
        assertEquals(overlap,value);
	}

	@Test
	void testSetStepsize() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		int stepSize = 2000;
		
		CloningTool cloningTool = new CloningTool();
		cloningTool.setStepsize(stepSize);
		
		assertNotNull(CloningTool.class.getDeclaredField("stepSize"));
		Field field = CloningTool.class.getDeclaredField("stepSize");
        field.setAccessible(true);
        int value = (int) field.get(cloningTool);
        assertEquals(stepSize,value);
        
        ////////
        
        assertNotNull(CloningTool.class.getDeclaredField("defaultOverlapRatio"));
		field = CloningTool.class.getDeclaredField("defaultOverlapRatio");
        field.setAccessible(true);
        double defaultOverlap = (double) field.get(cloningTool);
        
        int overlap = (int) (defaultOverlap*stepSize);
        
        assertNotNull(CloningTool.class.getDeclaredField("overlap"));
		field = CloningTool.class.getDeclaredField("overlap");
        field.setAccessible(true);
        value = (int) field.get(cloningTool);
        assertEquals(overlap,value);
	}
	
	@Test
	void testSetStepsizeAndOverlap() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		int stepSize = 2000;
		int overlap = 20;
		
		CloningTool cloningTool = new CloningTool();	
		cloningTool.setStepsizeAndOverlap(stepSize, overlap);
		
		// access private member
		assertNotNull(CloningTool.class.getDeclaredField("stepSize"));
		Field field = CloningTool.class.getDeclaredField("stepSize");
        field.setAccessible(true);
        int value = (int) field.get(cloningTool);
        assertEquals(stepSize,value);
        
        assertNotNull(CloningTool.class.getDeclaredField("overlap"));
		field = CloningTool.class.getDeclaredField("overlap");
        field.setAccessible(true);
        value = (int) field.get(cloningTool);
        assertEquals(overlap,value);
	}
	
	@Test
	void testCheckStepAndOverlap() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		CloningTool cloningTool = new CloningTool();	
		cloningTool.setStepsizeAndOverlap(1000, 10);	
		assertTrue(cloningTool.checkStepAndOverlap());
		
		// access private member
		assertNotNull(CloningTool.class.getDeclaredField("stepSize"));
		Field field = CloningTool.class.getDeclaredField("stepSize");
        field.setAccessible(true);
        field.set(cloningTool,10);
        
        assertNotNull(CloningTool.class.getDeclaredField("overlap"));
		field = CloningTool.class.getDeclaredField("overlap");
        field.setAccessible(true);
        field.set(cloningTool,100);
        
		assertFalse(cloningTool.checkStepAndOverlap());
	}
	
	@Test
	void testCheckStepAndOverlapErrorThrown() {
		
		CloningTool cloningTool = new CloningTool();			
		assertThrows(JPSRuntimeException.class,()->{cloningTool.setStepsizeAndOverlap(10, 100);});
		
		assertThrows(JPSRuntimeException.class,()->{new CloningTool(10, 100);});
	}
			
	@Test
	void testCloneFailed() {
		
		String message = "test error message";
		CloningTool cloningTool = new CloningTool();
		
		Throwable ex = assertThrows(JPSRuntimeException.class,()->{cloningTool.cloneFailed(message,10, 100);});
		assertTrue(ex.getMessage().contains(message));
	}
	
	@Test
	void testGetSparqlConstructNoBlanks() {
	
		int stepSize = 2000;
		int offset = 20;
		
		String expected = "CONSTRUCT {?s ?p ?o}\n"+
				"WHERE {?s ?p ?o."+
				"FILTER(  !isblank(?s) && !isblank(?o) )}\n"+
				"LIMIT "+Integer.toString(stepSize)+" OFFSET "+Integer.toString(offset);
		
		CloningTool cloningTool = new CloningTool();
		String query = cloningTool.getSparqlConstructNoBlanks(stepSize, offset);
			
		assertEquals(CloningToolTestHelper.removeWhiteSpace(expected), CloningToolTestHelper.removeWhiteSpace(query));
	}
	
	@Test
	void testGetSparqlConstructBlanks() {
		
		String expected = "CONSTRUCT {?s ?p ?o}\n"+
		"WHERE {?s ?p ?o."
		+ "FILTER(  isblank(?s) || isblank(?o) )}";
		
		CloningTool cloningTool = new CloningTool();
		String query = cloningTool.getSparqlConstructBlanks();
			
		assertEquals(CloningToolTestHelper.removeWhiteSpace(expected), CloningToolTestHelper.removeWhiteSpace(query));
	}
	
	@Test
	void testCountBlanks() {
		
		MockStoreClient storeClient = new MockStoreClient();
		
		int exptectedN = 1;
		storeClient.addTriple("<s1>", "<p1>", "<o1>");
		storeClient.addTriple("<s2>", "<p2>", "<o2>");
		storeClient.addTriple("_:b0", "<p3>", "<o3>"); //blank node
		
		CloningTool cloningTool = new CloningTool();
		int N = cloningTool.countBlanks(storeClient);
		assertEquals(exptectedN,N);
	}
	
	@Test
	void testPerformCloneStep() {

		int N = 10;
		
		MockStoreClient sourceStoreClient = new MockStoreClient();
		String sparqlInsert = CloningToolTestHelper.createInsertData(CloningToolTestHelper.createTriples(N));
		sourceStoreClient.executeUpdate(sparqlInsert);
		
		MockStoreClient targetStoreClient = new MockStoreClient();
		
		String query = "CONSTRUCT {?s ?p ?o}\n"
						+ "WHERE {?s ?p ?o.}";
		
		CloningTool cloningTool = new CloningTool();
		cloningTool.performCloneStep(sourceStoreClient,targetStoreClient,query);
		
		assertEquals(N, targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N, targetStoreClient));
	}
	
	@Test
	void testAdjustOverlap() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		int stepSize = 100;
		int overlap = 10;
		CloningTool cloningTool = new CloningTool(stepSize,overlap);
		
		assertNotNull(CloningTool.class.getDeclaredField("defaultOverlapRatio"));
		Field field = CloningTool.class.getDeclaredField("defaultOverlapRatio");
        field.setAccessible(true);
        double defaultOverlapRatio = (double) field.get(cloningTool);
		
        //error smaller than defaultOverlapRatio*stepSize
		int expectedOverlap = overlap + (int) (defaultOverlapRatio*stepSize);		 
		
		cloningTool.adjustOverlap(199, 200, 0);
		
        assertNotNull(CloningTool.class.getDeclaredField("overlap"));
        Field overlapField = CloningTool.class.getDeclaredField("overlap");
        overlapField.setAccessible(true);
        overlap = (int) overlapField.get(cloningTool);
        assertEquals(expectedOverlap,overlap);
		
		//large error
        int error = 50;
        expectedOverlap = overlap + error;

        cloningTool.adjustOverlap(300-error, 300, 1);
        overlap = (int) overlapField.get(cloningTool);
        assertEquals(expectedOverlap,overlap);
		
        //overlap becomes larger than stepsize
        assertThrows(JPSRuntimeException.class, ()->{cloningTool.adjustOverlap(200, 300, 1);});
        overlap = (int) overlapField.get(cloningTool);
        assertTrue(overlap>stepSize);
        
        //attempts > Max attempts
        assertThrows(JPSRuntimeException.class, ()->{cloningTool.adjustOverlap(200, 300, 5);});          
	}
	
	@Test
	void testCloneError() {
		
		//target store not empty
		MockStoreClient sourceStoreClient = new MockStoreClient();
		MockStoreClient targetStoreClient = new MockStoreClient();
		targetStoreClient.addTriple("<s>", "<p>", "<o>");		
		
		CloningTool cloningTool = new CloningTool();
		assertThrows(JPSRuntimeException.class, ()->{cloningTool.clone(sourceStoreClient, targetStoreClient);});
	}

	//See CloningToolIntegrationTest for more tests
	
	@Test
	void testClone() {

		int N = 10;
		
		MockStoreClient sourceStoreClient = new MockStoreClient();
		String sparqlInsert = CloningToolTestHelper.createInsertData(CloningToolTestHelper.createTriples(N));
		sourceStoreClient.executeUpdate(sparqlInsert);
		
		MockStoreClient targetStoreClient = new MockStoreClient();
		
		CloningTool cloningTool = new CloningTool();
		cloningTool.clone(sourceStoreClient,targetStoreClient);
		
		assertEquals(N, targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N, targetStoreClient));
	}
	
	@Test
	void testCloneWithBlanks() {

		int N = 10;
		
		MockStoreClient sourceStoreClient = new MockStoreClient();
		String sparqlInsert = CloningToolTestHelper.createInsertData(CloningToolTestHelper.createTriples(N));
		sourceStoreClient.executeUpdate(sparqlInsert);
		sourceStoreClient.addTriple("_:b0", "<predicate>", "<object>"); //blank node
		
		WhereBuilder where = new WhereBuilder();
		where.addWhere("_:b0", "<predicate>", "<object>");
		
		MockStoreClient targetStoreClient = new MockStoreClient();
		
		CloningTool cloningTool = new CloningTool();
		cloningTool.clone(sourceStoreClient,targetStoreClient);
		
		assertEquals(N+1, targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N, targetStoreClient));
		assertTrue(CloningToolTestHelper.checkSingleTriple(targetStoreClient, where));
	}
	
	@Test
	void testCloneWithNonEmptyTargetStore() {
		
		int N = 10;
		
		MockStoreClient sourceStoreClient = new MockStoreClient();
		String sparqlInsert = CloningToolTestHelper.createInsertData(CloningToolTestHelper.createTriples(N));
		sourceStoreClient.executeUpdate(sparqlInsert);
		
		MockStoreClient targetStoreClient = new MockStoreClient();
		targetStoreClient.addTriple("<subject>", "<predicate>", "<object>");
		
		WhereBuilder where = new WhereBuilder();
		where.addWhere("<subject>", "<predicate>", "<object>");
		
		CloningTool cloningTool = new CloningTool();
		cloningTool.overrideEmptyTarget();
		cloningTool.clone(sourceStoreClient,targetStoreClient);
		
		assertEquals(N+1, targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N, targetStoreClient));
		assertTrue(CloningToolTestHelper.checkSingleTriple(targetStoreClient, where));
	}
}
