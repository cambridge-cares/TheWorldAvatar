package uk.ac.cam.cares.jps.base.query.test;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.StoreRouter;

class StoreRouterTest {

	@Test
	void testIsFileBasedTargetResourceID() {		
		//Test different extensions
		assertTrue(StoreRouter.isFileBasedTargetResourceID("kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertTrue(StoreRouter.isFileBasedTargetResourceID("kb/sgp/singapore/SGTemperatureSensor-001.rdf"));
		assertTrue(StoreRouter.isFileBasedTargetResourceID("kb/sgp/singapore/SGTemperatureSensor-001.nt"));
		
		//Test IRI
		assertTrue(StoreRouter.isFileBasedTargetResourceID("http://theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		
		//Test not file based resources
		assertFalse(StoreRouter.isFileBasedTargetResourceID("http://theworldavatar.com/kb/ontokin"));
		assertFalse(StoreRouter.isFileBasedTargetResourceID("ontokin"));
		
		//Invalid file paths
		assertFalse(StoreRouter.isFileBasedTargetResourceID("kb/sgp:singapore/SGTemperatureSensor-001.owl"));
		assertFalse(StoreRouter.isFileBasedTargetResourceID("kb/sgp?singapore/SGTemperatureSensor-001.owl"));
		assertFalse(StoreRouter.isFileBasedTargetResourceID("kb/sgp*singapore/SGTemperatureSensor-001.owl"));
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
		//test getPath from file based target resource ID
		assertEquals("kb/sgp/singapore/SGTemperatureSensor-001.owl", StoreRouter.getPathComponent("kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertEquals("/kb/sgp/singapore/SGTemperatureSensor-001.owl", StoreRouter.getPathComponent("http://theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertEquals("/kb/sgp/singapore/SGTemperatureSensor-001.owl", StoreRouter.getPathComponent("http:///kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		assertEquals("/kb/sgp/singapore/SGTemperatureSensor-001.owl", StoreRouter.getPathComponent("http:/kb/sgp/singapore/SGTemperatureSensor-001.owl"));
		
		//test getPath with tomcat root path with file scheme
		assertEquals("/C:/TOMCAT/webapps/ROOT", StoreRouter.getPathComponent("file:///C:/TOMCAT/webapps/ROOT"));
	}
	
	@Test
	void testJoinPaths() {
		String path1 = "test/path/1";
		String path2 = "test/path/2";
		String expected = path1+"/"+path2;
		assertEquals(expected,StoreRouter.joinPaths(path1, path2));
		assertEquals(expected,StoreRouter.joinPaths(path1, "/"+path2));
	}
	
	@Test
	void testISRemoteTargetResourceID() {
		assertTrue(StoreRouter.isRemoteTargetResourceID("http://theworldavatar.com/kb/ontokin"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("ontokin"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("ontokin123"));
		assertTrue(StoreRouter.isRemoteTargetResourceID("123"));
		assertFalse(StoreRouter.isRemoteTargetResourceID("test/ontokin"));
		assertFalse(StoreRouter.isRemoteTargetResourceID("kb_ontokin"));
	}
	
}
