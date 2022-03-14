package uk.ac.cam.cares.jps.discovery.test;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.apache.http.client.ClientProtocolException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;

public class TestAgentLocator extends TestCase {

	public void testProperties() {
		String host = AgentLocator.getProperty("host");
		assertEquals("localhost", host);
	}

	public void testCallingAgentOneAndAgentTwo() throws ClientProtocolException, IOException {
		String response = AgentLocator.callAgent("agent.test.agentone");
		assertEquals("I am AgentOne and I am AgentTwo", response);
	}

	public void testRelativeDirectory() {
		String dir = AgentLocator.getAbsolutePath("reldir.workingdir", this);
		System.out.println("workingdir directory = " + dir);
		boolean b1 = dir.endsWith("/JPS_DISCOVERY/workingdir");
		boolean b2 = dir.endsWith("\\JPS_DISCOVERY\\workingdir");
		assertTrue(b1 || b2);
	}
	
	public void testRootDirectoryOnClientSide() throws UnsupportedEncodingException {
		String dir = AgentLocator.getCurrentJpsAppDirectory(this);
		boolean b1 = dir.endsWith("/JPS_DISCOVERY");
		boolean b2 = dir.endsWith("\\JPS_DISCOVERY");
		assertTrue(b1 || b2);
	}
	
	public void testRootDirectoryOnServerSide() throws ClientProtocolException, IOException {
		String dir = AgentLocator.callAgent("agent.test.agentreturningrootdirectory");
		boolean b1 = dir.endsWith("/JPS_DISCOVERY");
		boolean b2 = dir.endsWith("\\JPS_DISCOVERY");
		assertTrue(b1 || b2);
	}
}
