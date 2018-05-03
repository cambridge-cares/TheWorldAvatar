package uk.ac.cam.cares.jps.discovery.api;

public class AgentDescription extends AbstractAgentDescription {

	private static final long serialVersionUID = 4452519183268154498L;
	private TypeIRI address = null;
	
	public void addInputParameter(TypeIRI key) {
		Parameter param = new Parameter(key, null);
		inputParameters.add(param);
	}

	public TypeIRI getAddress() {
		return address;
	}

	public void setAddress(TypeIRI address) {
		this.address = address;
	}
}
