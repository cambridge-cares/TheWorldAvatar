package uk.ac.cam.cares.jps.composition.enginemodel;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.MatchingTool;

//@JsonIgnoreProperties(value = { "inputPool", "servicePool" }) // Variables in JsonIgnoreProperties will not be mapped to
// the JSONObject

public class Graph {

	public ArrayList<MessagePart> inputPool; // ignored in serialization
	public ArrayList<Service> servicePool; // ignored in serialization
	public ArrayList<Layer> layers;
	public ArrayList<MessagePart> initialInputs;
	public ArrayList<Edge> edges;
	public Map<String, Integer> updatesMap;

	public Graph() {
		this.initialInputs = new ArrayList<MessagePart>();
		this.inputPool = new ArrayList<MessagePart>();
		this.layers = new ArrayList<Layer>();
		this.servicePool = new ArrayList<Service>();
		this.edges = new ArrayList<Edge>();
		this.updatesMap = new HashMap<String, Integer>();
	}

	public void addLayer(Layer newLayer) {
		this.layers.add(newLayer);
	}

	public Service findServiceByInput(URI messagePart_URI) {
		Service result = null;
		for (Service service : this.servicePool) {
			for (MessagePart input : service.getAllInputs()) {
				if (MatchingTool.compareURI(messagePart_URI, input.uri)) {
					result = service;
				}
			}
		}
		return result;
	}

}
