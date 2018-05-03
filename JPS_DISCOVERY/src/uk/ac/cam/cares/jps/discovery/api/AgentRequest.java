package uk.ac.cam.cares.jps.discovery.api;

public class AgentRequest extends AbstractAgentDescription {

	private static final long serialVersionUID = -4861567149183876668L;

	public void addInputParameter(IType key, IType value) {
		Parameter param = new Parameter(key, value);
		inputParameters.add(param);
	}
}
