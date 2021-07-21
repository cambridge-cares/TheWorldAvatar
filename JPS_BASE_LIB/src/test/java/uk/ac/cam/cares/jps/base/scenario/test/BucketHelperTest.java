package uk.ac.cam.cares.jps.base.scenario.test;

import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import org.junit.Test;
import org.junit.function.ThrowingRunnable;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.*;

import org.junit.Rule;

public class BucketHelperTest{
	
	@Test
	public void TestgetScenarioUrl(){
		
		// Test getScenarioUrl(String scenarioName)
		String scenarioName = "testscenario";
		String serveraddress = KeyValueManager.getServerAddress(); 
		assertEquals(serveraddress + "/jps/scenario/testscenario", BucketHelper.getScenarioUrl(scenarioName));
		
		// Test getScenarioUrl()
		assertEquals(serveraddress + "/jps/scenario/base", BucketHelper.getScenarioUrl());
	}
	
	@Test
	public void TestgetScenarioName() {
		String TestscenarioUrl = "http://localhost:8080/jps/scenario/testscenario";
		assertEquals("testscenario", BucketHelper.getScenarioName(TestscenarioUrl));
	}
	
	@Test
	public void TestgetIriPrefix() {
		String Test_getIriPrefix = BucketHelper.getIriPrefix();
		int idx = Test_getIriPrefix.indexOf("/kb");
		String substringToTest = Test_getIriPrefix.substring(0,idx+"/kb".length());
		assertEquals("http://localhost:8080/jps/kb", substringToTest);
	}
	
	@Test
	public void TestgetUsecaseUrl() {
		String TestscenarioUrl = "http://localhost:8080/jps/scenario/testscenario";
		
		// Test getUsecaseUrl()
		String Test1_usecaseUrl = BucketHelper.getUsecaseUrl();
		int idx = Test1_usecaseUrl.indexOf("/kb/");
		String substringToTest = Test1_usecaseUrl.substring(0,idx+"/kb/".length());
		assertEquals("http://localhost:8080/jps/scenario/base/kb/", substringToTest);
		
		// Test getUsecaseUrl(String scenarioUrl)
		String Test2_usecaseUrl = BucketHelper.getUsecaseUrl(TestscenarioUrl);
		idx = Test2_usecaseUrl.indexOf("/kb/");
		substringToTest = Test2_usecaseUrl.substring(0,idx+"/kb/".length());
		assertEquals("http://localhost:8080/jps/scenario/testscenario/kb/", substringToTest);
	}
	
	@Test
	public void TestgetUsecaseUrlForData() {
		String Test_usecaseUrlforData = BucketHelper.getUsecaseUrlForData();
		int idx = Test_usecaseUrlforData.indexOf("/data/");
		String substringToTest = Test_usecaseUrlforData.substring(0,idx+"/data/".length());
		assertEquals("http://localhost:8080/jps/scenario/base/data/", substringToTest);
	}
	
	
	@Test(expected = JPSRuntimeException.class)
	public void TestgetKbScenarioUrl() {
		
		// Test if exception is thrown and its message
		Exception exception = assertThrows(JPSRuntimeException.class, new ThrowingRunnable() {
			public void run() throws Throwable {
				BucketHelper.getKbScenarioUrl();
			}
		});
		String expectedMessage = "can't create a scenario kb url for the base scenario";
	    String actualMessage = exception.getMessage();
	    assertTrue(actualMessage.contains(expectedMessage));
	    
	    // Test output of method getKbScenarioUrl()
		assertEquals("http://localhost:8080/jps/scenario/base/kb", BucketHelper.getKbScenarioUrl());
	}
	
	@Test
	public void TestisScenarioUrl() {
		// case True
		String testurl = "http://localhost:8080/jps/scenario/testscenario";
		assertTrue(BucketHelper.isScenarioUrl(testurl));
		// case False
		testurl = "test";
		assertFalse(BucketHelper.isScenarioUrl(testurl));
	}
	
	@Test
	public void TestisBaseScenario() {
		String testurl = null;
		assertTrue(BucketHelper.isBaseScenario(testurl));
		
		testurl = "http://localhost:8080/jps/scenario/testscenario";
		assertFalse(BucketHelper.isBaseScenario(testurl));
		
		testurl = "http://localhost:8080/jps/scenario/base";
		assertTrue(BucketHelper.isBaseScenario(testurl));
	}
	
	@Test
	public void TestgetLocalDataPath() {
		//TODO
	}
	
	@Test
	public void TestgetLocalDataPathWithoutThreadContext() {
		//TODO
	}
	
	@Test
	public void TestgetLocalPath123() {
		//TODO
	}
	
	
	
//	@Test(expected = JPSRuntimeException.class)
//	public void TestgetLocalPath() {
//		String testurl = "http://localhost:8080/jps/scenario/base";
//		String dataseturl = "test";
//		//BucketHelper.getLocalPath(testurl, dataseturl).throwJPSRuntimeException();
//	}
	
	
//	@Test
//	public void TestmapHost() {
//		URI testuri = URI.create("http://java.sun.com/j2se/1.3/");
//		assertEquals("here", BucketHelper.mapHost(testuri));
//	}
}