package uk.ac.cam.cares.jps.discovery.test;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import uk.ac.cam.cares.jps.base.discovery.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Agent;
import uk.ac.cam.cares.jps.base.discovery.AgentRequest;
import uk.ac.cam.cares.jps.base.discovery.AgentServiceDescription;
import uk.ac.cam.cares.jps.base.discovery.Parameter;

public class DescriptionFactory {

	private static String createType(String s) {
		if ("null".equals(s)) {
			return null;
		}
		return s;
	}
	
	private static List<Parameter> createParameters(String parameters) {
		
		List<Parameter> result = new ArrayList<Parameter>();
		
		StringTokenizer tokenizer = new StringTokenizer(parameters, ",");
		while (tokenizer.hasMoreTokens()) {	

			String key = tokenizer.nextToken();
			String value = tokenizer.nextToken();
			Parameter param = new Parameter(createType(key), createType(value));
			result.add(param);
		}
		
		return result;
	}
	
	private static void fill(AbstractAgentServiceDescription descr, String general, String input, String output) {
		
		List<Parameter> inputParameters = createParameters(input);
		descr.setInputParameters(inputParameters);
		List<Parameter> outputParameters = createParameters(output);
		descr.setOutputParameters(outputParameters);
		
		StringTokenizer tokenizer = new StringTokenizer(general, ",");
		while (tokenizer.hasMoreTokens()) {	
			String key = tokenizer.nextToken();
			String value = createType(tokenizer.nextToken());
			
			boolean attributeFound = true;
			if ("domain".equals(key)) {
				descr.setDomain(value);
			} else if (descr instanceof AgentServiceDescription) {
				attributeFound = false;
			} else {
				attributeFound = false;
			}
				
			if (!attributeFound) {
				System.out.println("Attribute was not found for agent description, key=" + key + ", value=" + value);
			}
		}
	}
	
	public static Agent createAgent(String name, String general, String input, String output) {
		Agent result = new Agent();
		result.setName(name);
		AgentServiceDescription descr = createAgentServiceDescription(general, input, output);
		result.addDescription(descr);	
		return result;
	}
	
	public static AgentServiceDescription createAgentServiceDescription(String general, String input, String output) {
		AgentServiceDescription result = new AgentServiceDescription();
		fill(result, general, input, output);
		return result;
	}
	
	static AgentRequest createDiscoveryMessage(String general, String input, String output) {
		AgentRequest result = new AgentRequest();
		fill(result, general, input, output);
		return result;
	}
}
