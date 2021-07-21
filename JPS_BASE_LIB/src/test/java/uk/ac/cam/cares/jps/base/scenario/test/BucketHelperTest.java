package uk.ac.cam.cares.jps.base.scenario.test;

import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;

import org.junit.Test;
import org.junit.function.ThrowingRunnable;

import static org.junit.Assert.*;

public class BucketHelperTest{
	
	// Test getScenarioUrl 
	@Test
	public void TestgetScenarioUrl(){
		
		// Test getScenarioUrl(String scenarioName)
		String scenarioName = "testscenario";
		String serveraddress = KeyValueManager.getServerAddress(); 
		assertEquals(serveraddress + "/jps/scenario/testscenario", BucketHelper.getScenarioUrl(scenarioName));
		
		// Test getScenarioUrl()
		assertEquals(serveraddress + "/jps/scenario/base", BucketHelper.getScenarioUrl());
	}
	
	
	// Test getScenarioName
	@Test
	public void TestgetScenarioName() {
		String TestscenarioUrl = "http://localhost:8080/jps/scenario/testscenario";
		assertEquals("testscenario", BucketHelper.getScenarioName(TestscenarioUrl));
	}
	
	
	// Test getIriPrefix
	@Test
	public void TestgetIriPrefix() {
		String Test_getIriPrefix = BucketHelper.getIriPrefix();
		
		// Break output into substring, exclude random UUID
		int idx = Test_getIriPrefix.indexOf("/kb");
		String substringToTest = Test_getIriPrefix.substring(0,idx+"/kb".length());
		assertEquals("http://localhost:8080/jps/kb", substringToTest);
	}
	
	
	// Test getUsecaseUrl
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
	
	
	// Test getUsecaseUrlForData
	@Test
	public void TestgetUsecaseUrlForData() {
		String Test_usecaseUrlforData = BucketHelper.getUsecaseUrlForData();
		int idx = Test_usecaseUrlforData.indexOf("/data/");
		String substringToTest = Test_usecaseUrlforData.substring(0,idx+"/data/".length());
		assertEquals("http://localhost:8080/jps/scenario/base/data/", substringToTest);
	}
	
	
	// Test getKbScenarioUrl
	@Test(expected = JPSRuntimeException.class)
	public void TestgetKbScenarioUrl() throws JPSRuntimeException {
		
		// Test if exception is thrown 
		Exception exception = assertThrows(JPSRuntimeException.class, new ThrowingRunnable() {
			public void run() throws Throwable {
				BucketHelper.getKbScenarioUrl();
			}
		});
		// Test if exception message is as expected
		String expectedMessage = "can't create a scenario kb url for the base scenario";
	    String actualMessage = exception.getMessage();
	    assertTrue(actualMessage.contains(expectedMessage));
	    
	    // Test output of method getKbScenarioUrl()
		assertEquals("http://localhost:8080/jps/scenario/base/kb", BucketHelper.getKbScenarioUrl());
	}
	
	
	// Test isScenarioUrl
	@Test
	public void TestisScenarioUrl() {
		// case True
		String testurl = "http://localhost:8080/jps/scenario/testscenario";
		assertTrue(BucketHelper.isScenarioUrl(testurl));
		// case False
		testurl = "test";
		assertFalse(BucketHelper.isScenarioUrl(testurl));
	}
	
	
	// Test isBaseScenario
	@Test
	public void TestisBaseScenario() {
		String testurl = null;
		assertTrue(BucketHelper.isBaseScenario(testurl));
		
		testurl = "http://localhost:8080/jps/scenario/testscenario";
		assertFalse(BucketHelper.isBaseScenario(testurl));
		
		testurl = "http://localhost:8080/jps/scenario/base";
		assertTrue(BucketHelper.isBaseScenario(testurl));
	}
	
	
	// Test getLocalDataPath
	@Test
	public void TestgetLocalDataPath() {
		String test_localDataPath = BucketHelper.getLocalDataPath();
		String scenario_workingdir = ScenarioHelper.getScenarioWorkingDir();
		int idx = test_localDataPath.indexOf("/data/");
		String substringToTest = test_localDataPath.substring(0,idx+"/data/".length());
		assertEquals(scenario_workingdir + "/base/localhost_8080/data/", substringToTest);
	}
	
	
	// Test getLocalDataPathWithoutThreadContext
	@Test
	public void TestgetLocalDataPathWithoutThreadContext() {
		String test_LocalDataPathWithoutThreadContext = BucketHelper.getLocalDataPathWithoutThreadContext();
		String scenario_workingdir = ScenarioHelper.getScenarioWorkingDir();
		int idx = test_LocalDataPathWithoutThreadContext.indexOf("/data/");
		String substringToTest = test_LocalDataPathWithoutThreadContext.substring(0,idx+"/data/".length());
		assertEquals(scenario_workingdir + "/base/localhost_8080/data/", substringToTest);
	}
	
	
	// Test getLocalPath(String url)
	@Test
	public void TestgetLocalPath_singleinput() {
		String test_url = "http://localhost:8080/jps/scenario/base";
		String scenario_workingdir = ScenarioHelper.getScenarioWorkingDir();
		assertEquals(scenario_workingdir + "/base", BucketHelper.getLocalPath(test_url));
	}
	
	
	// Test getLocalPath(String url, String datasetUrl)
	@Test
	public void TestgetLocalPath() {
		
		// Exception not thrown case
	    String test_url = "http://localhost:8080/jps/scenario/base";
	    String test_dataseturl = "http://localhost:8080/jps/dataset/testdata";
	    String workingdir = ScenarioHelper.getJpsWorkingDir();
	    assertEquals(workingdir + "/JPS_SCENARIO/dataset/testdata/localhost_8080/jps/scenario/base", BucketHelper.getLocalPath(test_url, test_dataseturl));
		
		// Test if exception is thrown 
		Exception exception = assertThrows(JPSRuntimeException.class, new ThrowingRunnable() {
			public void run() throws Throwable {
				BucketHelper.getLocalPath("http://localhost:8080/jps/scenario/base", "test");
			}
		});
		
		// Test exception message is as expected
		String expectedMessage = "unknown datasetUrl=test, url=http://localhost:8080/jps/scenario/base"; 
	    String actualMessage = exception.getMessage();
	    assertTrue(actualMessage.contains(expectedMessage));
	    
	    // Test various if statements
	    test_dataseturl = null;
	    String scenario_workingdir = ScenarioHelper.getScenarioWorkingDir();
	    assertEquals(scenario_workingdir + "/base", BucketHelper.getLocalPath(test_url, test_dataseturl));
	    
	    test_url = "http://localhost:8080/data/jps/scenario/base";
	    assertEquals(ResourcePathConverter.convertToLocalPath(test_url), BucketHelper.getLocalPath(test_url, test_dataseturl));

	    test_url = "http://localhost:8080/jps/testscenario";
	    String scenarioBucket = ScenarioHelper.getScenarioBucket("base");
	    assertEquals(scenarioBucket + "/localhost_8080/testscenario", BucketHelper.getLocalPath(test_url, test_dataseturl));		
	}
}