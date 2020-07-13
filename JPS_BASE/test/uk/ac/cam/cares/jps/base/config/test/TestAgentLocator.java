package uk.ac.cam.cares.jps.base.config.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;

public class TestAgentLocator extends TestCase {

	public void testIsVersionLargerThan() {
		assertFalse(AgentLocator.isVersionLargerThan(null, "0.0.0"));
		assertTrue(AgentLocator.isVersionLargerThan("0.0.0", null));
		
		assertTrue(AgentLocator.isVersionLargerThan("0.02.0", "0.0.1"));
		assertTrue(AgentLocator.isVersionLargerThan("0.0.99", "0.0.9"));
		assertTrue(AgentLocator.isVersionLargerThan("0.02.0", "0.0.0"));
		assertTrue(AgentLocator.isVersionLargerThan("1.02.0", "0.0.0"));
		assertTrue(AgentLocator.isVersionLargerThan("1.02.1", "1.2.0"));
		assertTrue(AgentLocator.isVersionLargerThan("0.02.1", "0.2.0"));
		assertTrue(AgentLocator.isVersionLargerThan("0.0.98", "0.0.97"));
		assertTrue(AgentLocator.isVersionLargerThan("0.04.0", "0.3.0"));
		assertTrue(AgentLocator.isVersionLargerThan("0.4.6", "0.4.5"));
		assertTrue(AgentLocator.isVersionLargerThan("3.2.1", "3.2.0"));
		assertTrue(AgentLocator.isVersionLargerThan("3.2.0", "3.1.5"));
		
		
		assertFalse(AgentLocator.isVersionLargerThan("0.0.98", "0.0.99"));
		assertFalse(AgentLocator.isVersionLargerThan("2.2.0", "2.2.1"));
		assertFalse(AgentLocator.isVersionLargerThan("2.2.0", "2.3.0"));
		assertFalse(AgentLocator.isVersionLargerThan("2.2.0", "3.0.0"));
		assertFalse(AgentLocator.isVersionLargerThan("0.0.9", "0.0.90"));
		assertFalse(AgentLocator.isVersionLargerThan("0.55.55", "0.55.55"));
		assertFalse(AgentLocator.isVersionLargerThan("5.5.5", "6.0.0"));
		assertFalse(AgentLocator.isVersionLargerThan("0.02.0", "0.2.0"));
		assertFalse(AgentLocator.isVersionLargerThan("0.02.0", "0.02.1"));
	}
	
	public void excludedtestJpsBaseDir() {
		String path = "C:\\Users\\Andreas\\Tomcat8_New\\webapps\\JPS_BASE##1.0.0";
		path = AgentLocator.createJpsBaseDirectory(path);
		assertEquals("C:/Users/Andreas/Tomcat8_New/webapps/JPS_BASE##1.0.0", path);
	}
	
}
