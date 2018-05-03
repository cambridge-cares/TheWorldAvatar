package uk.ac.cam.cares.jps.discovery.test;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import uk.ac.cam.cares.jps.discovery.api.AbstractAgentDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.api.IType;
import uk.ac.cam.cares.jps.discovery.api.Parameter;
import uk.ac.cam.cares.jps.discovery.api.TypeIRI;
import uk.ac.cam.cares.jps.discovery.api.TypeString;

public class DescriptionFactory {

	private static IType createType(String s) {
		if (s.startsWith("IRI") || s.startsWith("http") ) {
			return new TypeIRI(s);
		} else if ("null".equals(s)) {
			return null;
		}
		return new TypeString(s);
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
	
	private static void fill(AbstractAgentDescription descr, String general, String input, String output) {
		
		List<Parameter> inputParameters = createParameters(input);
		descr.setInputParameters(inputParameters);
		List<Parameter> outputParameters = createParameters(output);
		descr.setOutputParameters(outputParameters);
		
		StringTokenizer tokenizer = new StringTokenizer(general, ",");
		while (tokenizer.hasMoreTokens()) {	
			String key = tokenizer.nextToken();
			IType value = createType(tokenizer.nextToken());
			
			boolean attributeFound = true;
			if ("domain".equals(key)) {
				descr.setDomain(value);
			} else if (descr instanceof AgentDescription) {
				if ("address".equals(key)) {
					((AgentDescription) descr).setAddress((TypeIRI) value);
				} else {
					attributeFound = false;
				}
			} else {
				attributeFound = false;
			}
				
			if (!attributeFound) {
				System.out.println("Attribute was not find for agent description, key=" + key + ", value=" + value);
			}
		}
	}
	
	static AgentDescription createAgentDescription(String general, String input, String output) {
		AgentDescription result = new AgentDescription();
		fill(result, general, input, output);
		return result;
	}
	
	static AgentRequest createDiscoveryMessage(String general, String input, String output) {
		AgentRequest result = new AgentRequest();
		fill(result, general, input, output);
		return result;
	}
}
