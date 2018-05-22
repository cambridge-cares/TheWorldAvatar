package uk.ac.cam.cares.jps.base.discovery;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Agent implements Serializable {

	private static final long serialVersionUID = 4541394875847067036L;
	TypeIRI name = null;
	List<AgentServiceDescription> descriptions = new ArrayList<AgentServiceDescription>();
	
	public TypeIRI getName() {
		return name;
	}
	
	public void setName(TypeIRI name) {
		this.name = name;
	}
	
	public List<AgentServiceDescription> getDescriptions() {
		return descriptions;
	}

	public void addDescription(AgentServiceDescription descr) {
		descriptions.add(descr);
	}
}
