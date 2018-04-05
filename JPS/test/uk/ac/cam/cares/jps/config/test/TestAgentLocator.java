package uk.ac.cam.cares.jps.config.test;

import java.io.File;
import java.io.IOException;

import org.apache.http.client.ClientProtocolException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.config.AgentLocator;

public class TestAgentLocator extends TestCase {

	public void testProperties() {
		String host = AgentLocator.getProperty("host");
		assertEquals("http://localhost", host);
	}

	public void testCallingAgentOneAndAgentTwo() throws ClientProtocolException, IOException {
		String response = AgentLocator.callAgent("agent.test.agentone");
		assertEquals("I am AgentOne and I am AgentTwo", response);
	}

	public void testRelativeDirectory() {
		String directory = AgentLocator.getAbsolutePath("reldir.python");
		System.out.println("python directory = " + directory);
		File file = new File(directory);
		assertTrue(file.exists());
	}
}
