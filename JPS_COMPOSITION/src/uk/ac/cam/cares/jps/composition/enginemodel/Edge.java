package uk.ac.cam.cares.jps.composition.enginemodel;

import java.net.URI;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import uk.ac.cam.cares.jps.composition.servicemodel.Service;
@JsonIgnoreProperties(value = {"fromNode", "toNode"})

public class Edge {

	public Service fromNode; 
	public Service toNode;	

	public URI fromNodeURI; 
	public URI toNodeURI;	
	public int [] fromOutput;
	public int [] toInput;
	public String name;
	
	public Edge() {
		
	}
	
	
}
