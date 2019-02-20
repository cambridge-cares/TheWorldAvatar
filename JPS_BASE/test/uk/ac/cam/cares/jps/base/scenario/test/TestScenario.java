package uk.ac.cam.cares.jps.base.scenario.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

public class TestScenario extends TestCase {

	public void testDividePath() {
		
		// auto generate a scenario id
		String path = "/";
		String[] actual= ScenarioHelper.dividePath(path);
		assertNotNull(actual[0]);
		assertNull(actual[1]);
		
		// throws an exception because path (without /) does not contain at least 10 characters
		path = "/123456789";
		try {
			ScenarioHelper.dividePath(path);
		} catch (Exception e) {
		}
	
		path = "/1234567890";
		actual= ScenarioHelper.dividePath(path);
		assertEquals("1234567890", actual[0]);
		assertNull(actual[1]);
		
		path = "/1234567890/any/fancy/operation";
		actual= ScenarioHelper.dividePath(path);
		assertEquals("1234567890", actual[0]);
		assertEquals("/any/fancy/operation", actual[1]);
	}
}
