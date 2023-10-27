package uk.ac.cam.cares.jps.base.scenario;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.regex.Pattern;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.function.ThrowingRunnable;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;

public class BucketHelperTest {

	// Test getScenarioUrl()
	@Test
	public void TestgetScenarioUrl() {

		// Test getScenarioUrl(String scenarioName)
		String scenarioName = "testscenario";
		String serveraddress = KeyValueManager.getServerAddress();
		assertEquals(serveraddress + "/jps/scenario/testscenario", BucketHelper.getScenarioUrl(scenarioName));

		// Test getScenarioUrl()
		assertEquals(serveraddress + "/jps/scenario/base", BucketHelper.getScenarioUrl());
	}

	// Test getScenarioName()
	@Test
	public void TestgetScenarioName() {
		String TestscenarioUrl = "http://localhost:8080/jps/scenario/testscenario";
		assertEquals("testscenario", BucketHelper.getScenarioName(TestscenarioUrl));
	}

	// Test getIriPrefix()
	@Test
	public void TestgetIriPrefix() {
		String Test_getIriPrefix = BucketHelper.getIriPrefix();

		// Break output into substring, exclude random UUID
		int idx = Test_getIriPrefix.indexOf("/kb");
		String substringToTest = Test_getIriPrefix.substring(0, idx + "/kb/".length());
		String serveraddress = KeyValueManager.getServerAddress();
		assertEquals(serveraddress + "/jps/kb/", substringToTest);

		// Test if UUID is present in the final output
		String testIfUUIDpresent = Test_getIriPrefix.substring(idx + "/kb/".length());
		final Pattern UUID_REGEX_PATTERN = Pattern
				.compile("^[{]?[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}[}]?$");

		assertTrue(UUID_REGEX_PATTERN.matcher(testIfUUIDpresent).matches());

	}

	// Test getUsecaseUrl()
	@Test
	public void TestgetUsecaseUrl() {

		// Break output into substring, exclude random UUID
		String Test_usecaseUrl = BucketHelper.getUsecaseUrl();
		int idx = Test_usecaseUrl.indexOf("/kb/");
		String substringToTest = Test_usecaseUrl.substring(0, idx + "/kb/".length());
		String serveraddress = KeyValueManager.getServerAddress();
		assertEquals(serveraddress + "/jps/scenario/base/kb/", substringToTest);

		// Test if UUID is present in the final output
		String testIfUUIDpresent = Test_usecaseUrl.substring(idx + "/kb/".length());
		final Pattern UUID_REGEX_PATTERN = Pattern
				.compile("^[{]?[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}[}]?$");

		assertTrue(UUID_REGEX_PATTERN.matcher(testIfUUIDpresent).matches());

	}

	// Test getUsecaseUrl(String scenarioUrl)
	@Test
	public void getUsecaseUrl_stringInput() {
		String TestscenarioUrl = "http://localhost:8080/jps/scenario/testscenario";

		// Break output into substring, exclude random UUID
		String Test_usecaseUrl = BucketHelper.getUsecaseUrl(TestscenarioUrl);
		int idx = Test_usecaseUrl.indexOf("/kb/");
//		String substringToTest = Test_usecaseUrl.substring(0, idx + "/kb/".length());
//		String serveraddress = KeyValueManager.getServerAddress();
//		assertEquals(serveraddress + "/jps/scenario/testscenario/kb/", substringToTest);

		// Test if UUID is present in the final output
		String testIfUUIDpresent = Test_usecaseUrl.substring(idx + "/kb/".length());
		final Pattern UUID_REGEX_PATTERN = Pattern
				.compile("^[{]?[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}[}]?$");

		assertTrue(UUID_REGEX_PATTERN.matcher(testIfUUIDpresent).matches());
	}

	// Test getUsecaseUrlForData
	@Test
	public void TestgetUsecaseUrlForData() {
		String Test_usecaseUrlforData = BucketHelper.getUsecaseUrlForData();

		// Break output into substring, exclude random UUID
		int idx = Test_usecaseUrlforData.indexOf("/data/");
		String substringToTest = Test_usecaseUrlforData.substring(0, idx + "/data/".length());
		String serveraddress = KeyValueManager.getServerAddress();
		assertEquals(serveraddress + "/jps/scenario/base/data/", substringToTest);

		// Test if UUID is present in the final output
		String testIfUUIDpresent = Test_usecaseUrlforData.substring(idx + "/data/".length());
		final Pattern UUID_REGEX_PATTERN = Pattern
				.compile("^[{]?[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}[}]?$");

		assertTrue(UUID_REGEX_PATTERN.matcher(testIfUUIDpresent).matches());
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
		String serveraddress = KeyValueManager.getServerAddress();
		assertEquals(serveraddress + "/jps/scenario/base/kb", BucketHelper.getKbScenarioUrl());
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

	// Test getLocalDataPath - to be fixed after clarifying purpose of getLocalDataPath()
	@Ignore
	public void TestgetLocalDataPath() {
		String test_localDataPath = BucketHelper.getLocalDataPath();
		String scenario_workingdir = ScenarioHelper.getScenarioWorkingDir();

		// Break output into substring, exclude random UUID
		int idx = test_localDataPath.indexOf("/data/");
		String substringToTest = test_localDataPath.substring(0, idx + "/data/".length());
		String serveraddress = KeyValueManager.getServerAddress();
		assertEquals(scenario_workingdir + "/base/localhost_8080/data/", substringToTest);

		// Test if UUID is present in the final output
		String testIfUUIDpresent = test_localDataPath.substring(idx + "/data/".length());
		final Pattern UUID_REGEX_PATTERN = Pattern
				.compile("^[{]?[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}[}]?$");

		assertTrue(UUID_REGEX_PATTERN.matcher(testIfUUIDpresent).matches());

	}

	// Test getLocalDataPathWithoutThreadContext - to be fixed after clarifying purpose of getLocalDataPathWithoutThreadContext()
	@Ignore
	public void TestgetLocalDataPathWithoutThreadContext() {
		String test_LocalDataPathWithoutThreadContext = BucketHelper.getLocalDataPathWithoutThreadContext();
		String scenario_workingdir = ScenarioHelper.getScenarioWorkingDir();

		// Break output into substring, exclude random UUID
		int idx = test_LocalDataPathWithoutThreadContext.indexOf("/data/");
		String substringToTest = test_LocalDataPathWithoutThreadContext.substring(0, idx + "/data/".length());
		assertEquals(scenario_workingdir + "/base/localhost_8080/data/", substringToTest);

		// Test if UUID is present in the final output
		String testIfUUIDpresent = test_LocalDataPathWithoutThreadContext.substring(idx + "/data/".length());
		final Pattern UUID_REGEX_PATTERN = Pattern
				.compile("^[{]?[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}[}]?$");

		assertTrue(UUID_REGEX_PATTERN.matcher(testIfUUIDpresent).matches());
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
		assertEquals(workingdir + "/JPS_SCENARIO/dataset/testdata/localhost_8080/jps/scenario/base",
				BucketHelper.getLocalPath(test_url, test_dataseturl));

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
		MockedStatic<KeyValueManager> mock = Mockito.mockStatic(KeyValueManager.class);
		assertEquals(ResourcePathConverter.convertToLocalPath(test_url),
				BucketHelper.getLocalPath(test_url, test_dataseturl));
		mock.close();

		test_url = "http://localhost:8080/jps/testscenario";
		String scenarioBucket = ScenarioHelper.getScenarioBucket("base");
		assertEquals(scenarioBucket + "/localhost_8080/testscenario",
				BucketHelper.getLocalPath(test_url, test_dataseturl));
	}

	// Test for private method mapHost(URI uri)
	@Test
	public void TestmapHost() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		BucketHelper bh = new BucketHelper();
		assertNotNull(bh.getClass().getDeclaredMethod("mapHost", URI.class));
		Method mapHost = bh.getClass().getDeclaredMethod("mapHost", URI.class);
		mapHost.setAccessible(true);

		// Test return of method if port exists
		String str = "http://localhost:8080/jps/scenario/testscenario";
		URI uri = URI.create(str);
		String test_mapped = (String) mapHost.invoke(null, uri);
		assertEquals("localhost_8080", test_mapped);

		// Test return if port does not exist
		str = "http://www.google.com";
		uri = URI.create(str);
		test_mapped = (String) mapHost.invoke(null, uri);
		assertEquals("www_google_com", test_mapped);

	}
}