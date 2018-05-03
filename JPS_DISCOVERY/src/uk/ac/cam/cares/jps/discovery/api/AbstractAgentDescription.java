package uk.ac.cam.cares.jps.discovery.api;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


public abstract class AbstractAgentDescription implements Serializable {

	private static final long serialVersionUID = 2482265137462822690L;
	private IType domain = null;
	protected List<Parameter> inputParameters = new ArrayList<Parameter>();
	private List<Parameter> outputParameters = new ArrayList<Parameter>();
	
	public IType getDomain() {
		return domain;
	}

	public void setDomain(IType domain) {
		this.domain = domain;
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

	public void addOutputParameter(TypeIRI key) {
		Parameter param = new Parameter(key, null);
		outputParameters.add(param);
	}

	public void setOutputParameters(List<Parameter> outputParameters) {
		this.outputParameters = outputParameters;
	}
	
	public static void copy(AbstractAgentDescription source, AbstractAgentDescription dest) {
		//TODO-AE URGENT. This is not a deep copy!!! If a output value is set in a copied response
		// it is also set in the request
		dest.setDomain(source.getDomain());
		dest.setInputParameters(source.getInputParameters());
		dest.setOutputParameters(source.getOutputParameters());
	}
}