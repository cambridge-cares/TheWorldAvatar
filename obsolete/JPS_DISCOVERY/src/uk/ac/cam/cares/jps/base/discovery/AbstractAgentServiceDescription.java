package uk.ac.cam.cares.jps.base.discovery;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


public abstract class AbstractAgentServiceDescription implements Serializable {

	private static final long serialVersionUID = 2482265137462822690L;
	private List<Parameter> properties = new ArrayList<Parameter>();
	protected List<Parameter> inputParameters = new ArrayList<Parameter>();
	private List<Parameter> outputParameters = new ArrayList<Parameter>();

	public List<Parameter> getProperties() {
		return properties;
	}

	public void setProperties(List<Parameter> properties) {
		this.properties = properties;
	}
	
	public void setDomain(String domain) {
		Parameter param = new Parameter(new String("domain"), domain);
		properties.add(param);
	}

	public List<Parameter> getInputParameters() {
		return inputParameters;
	}
	
	public void setInputParameters(List<Parameter> inputParameters) {
		this.inputParameters = inputParameters;
	}
	
	public List<Parameter> getOutputParameters() {
		return outputParameters;
	}

	public void setOutputParameters(List<Parameter> outputParameters) {
		this.outputParameters = outputParameters;
	}
	
	public void addOutputParameter(String key) {
		Parameter param = new Parameter(key, null);
		outputParameters.add(param);
	}
	
	public static void copyParameters(AbstractAgentServiceDescription source, AbstractAgentServiceDescription dest) {
		dest.setProperties(copy(source.getProperties()));
		dest.setInputParameters(copy(source.getInputParameters()));
		dest.setOutputParameters(copy(source.getOutputParameters()));
	}
	
	private static List<Parameter> copy(List<Parameter> params) {
		List<Parameter> result = new ArrayList<Parameter>();
		
		for (Parameter current : params) {
			Parameter copiedParam = new Parameter(current.getKey(), current.getValue());
			result.add(copiedParam);
		}
		
		return result;
	}
}