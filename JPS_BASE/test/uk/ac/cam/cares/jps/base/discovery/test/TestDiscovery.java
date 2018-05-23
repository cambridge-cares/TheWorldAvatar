package uk.ac.cam.cares.jps.base.discovery.test;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.Parameter;
import uk.ac.cam.cares.jps.base.discovery.TypeString;

public class TestDiscovery extends TestCase {
	
	public void testSerializeAgentReqestWithJson() {
		
		AgentRequest expected = new AgentRequest();
		Parameter prop = new Parameter(new TypeString("myPropertyKey"), new TypeString("myPropertyValue"));
		expected.getProperties().add(prop);
		Parameter paramIn = new Parameter(new TypeString("myInputKey"), new TypeString("myInputValue"));
		expected.getInputParameters().add(paramIn);
		Parameter paramOut = new Parameter(new TypeString("myOutputKey"), new TypeString("myOutputValue"));
		expected.getOutputParameters().add(paramOut);
		
		Gson gson = new Gson();
		String s = gson.toJson(expected);
		System.out.println("serialized = " + s);
		
		AgentRequest actual = gson.fromJson(s, AgentRequest.class);
		
		assertTrue(prop.getKey().equals(actual.getProperties().get(0).getKey()));
		assertTrue(prop.getValue().equals(actual.getProperties().get(0).getValue()));
		assertTrue(paramIn.getKey().equals(actual.getInputParameters().get(0).getKey()));
		assertTrue(paramIn.getValue().equals(actual.getInputParameters().get(0).getValue()));
		assertTrue(paramOut.getKey().equals(actual.getOutputParameters().get(0).getKey()));
		assertTrue(paramOut.getValue().equals(actual.getOutputParameters().get(0).getValue()));
	}
}
