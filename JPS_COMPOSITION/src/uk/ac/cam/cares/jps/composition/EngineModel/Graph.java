package uk.ac.cam.cares.jps.composition.EngineModel;

import java.net.URI;
import java.util.ArrayList;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import uk.ac.cam.cares.jps.composition.ServiceModel.*;
@JsonIgnoreProperties(value = {"inputPool","servicePool"}) //Variables in JsonIgnoreProperties will not be mapped to the JSONObject 

public class Graph {
	
	public ArrayList<MessagePart> inputPool; // ignored in serialization
	public ArrayList<Service> servicePool; // ignored in serialization
	public ArrayList<Layer> layers; 
	public ArrayList<MessagePart> initialInputs;
	
	
	public Graph() {
		this.initialInputs = new ArrayList<MessagePart>();
		this.inputPool = new ArrayList<MessagePart>();
		this.layers = new ArrayList<Layer>();
		this.servicePool = new ArrayList<Service>();
	}
	
	public void addLayer(Layer newLayer) {
		this.layers.add(newLayer);
	}
	
	
	
}
