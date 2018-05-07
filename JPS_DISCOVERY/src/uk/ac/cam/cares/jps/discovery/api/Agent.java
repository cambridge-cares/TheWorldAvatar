package uk.ac.cam.cares.jps.discovery.api;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Agent implements Serializable {

	private static final long serialVersionUID = 4541394875847067036L;
	TypeIRI name = null;
	List<AgentDescription> descriptions = new ArrayList<AgentDescription>();
	
	public TypeIRI getName() {
		return name;
	}
	
	public void setName(TypeIRI name) {
		this.name = name;
	}
	
	public List<AgentDescription> getDescriptions() {
		return descriptions;
	}

	public void addDescription(AgentDescription descr) {
		descriptions.add(descr);
	}
}
