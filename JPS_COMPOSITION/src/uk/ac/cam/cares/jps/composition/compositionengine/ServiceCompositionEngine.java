package uk.ac.cam.cares.jps.composition.compositionengine;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;

import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.enginemodel.Layer;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.MatchingTool;
import uk.ac.cam.cares.jps.composition.util.OptimalPathSearcher;
import uk.ac.cam.cares.jps.composition.webserver.ServiceDiscovery;

public class ServiceCompositionEngine {

	public String fullHostName = "";
	public Graph newGraph;
	private ServiceDiscovery serviceDiscovery;
	private ArrayList<MessagePart> inputsToAppend;
	private ArrayList<URI> outputsRequired;

	public ServiceCompositionEngine(Service compositeAgent, String host) throws Exception {
		this.newGraph = new Graph();
		this.fullHostName = host;
		this.newGraph.initialInputs = (ArrayList<MessagePart>) compositeAgent.getAllInputs();
		this.serviceDiscovery = new ServiceDiscovery(this.fullHostName);
		this.inputsToAppend = new ArrayList<MessagePart>();
		this.outputsRequired = new ArrayList<URI>();

		System.out.println("Here 002");

		for (MessagePart part : compositeAgent.getAllOutputs()) {
			this.outputsRequired.add(part.getModelReference());
		}
		this.inputsToAppend = (ArrayList<MessagePart>) compositeAgent.getAllInputs();
	}

	public Graph getGraph() {
		return this.newGraph;
	}

	public boolean appendLayerToGraph(int index) {
		// Receive a set of inputs
		// Find all the candidate basing on the inputs
		// Increment the index, declare a new layer, put all the candidates in the layer

		this.newGraph.inputPool.addAll(this.inputsToAppend); // the input come from the result from last iteration
		inputsToAppend = new ArrayList<MessagePart>();
		ArrayList<Service> servicesToAppend = this.serviceDiscovery.getAllServiceCandidates(this.newGraph.inputPool,
				this.newGraph.servicePool);
		this.newGraph.servicePool.addAll(servicesToAppend);
		for (Service service : servicesToAppend) {
			this.inputsToAppend.addAll(service.getAllOutputs());
		}
		Layer newLayer = new Layer(index);
		newLayer.setServices(servicesToAppend);
		this.newGraph.addLayer(newLayer);

		boolean metRequirement = true; // If one of the requirement is not me, the boolean value will be set to false.

		for (URI required : this.outputsRequired) {
			boolean atLeastOneHit = false;
			for (MessagePart output : this.inputsToAppend) {
				URI modelRef = output.getModelReference(); // get the newly generated output and compare it to the
															// desired output.
				if (modelRef.toString().contentEquals(required.toString())) {
					atLeastOneHit = true;
				}
			}
			if (!atLeastOneHit) {
				metRequirement = false;
			}
		}
		return metRequirement;
	}

	public int eliminateRedundantAgent() {
		// iterate through each agent
		// get the pool of inputs, if one agent has no outputs connecting to inputs pool
		// and has no outputs in expected outputs, eliminate it.
		HashMap<Layer, ArrayList<Service>> executionList = new HashMap<Layer, ArrayList<Service>>();
		for (Layer layer : this.newGraph.layers) {
			for (Service service : layer.getServices()) {
				boolean leadToNoInput = true;
				for (Layer layer_b : this.newGraph.layers) {
					for (MessagePart part : layer_b.getAllInputs()) {
						for (MessagePart service_part : service.getAllOutputs()) {
							if (MatchingTool.compareURI(service_part.getModelReference(), part.getModelReference())) {
								leadToNoInput = false; // Do not eliminate the agent
							}
							for (URI output : this.outputsRequired) {
								if (MatchingTool.compareURI(service_part.getModelReference(), output)) {
									leadToNoInput = false; // Do not eliminate the agent
								}
							} // As long as there is one match, the service will survive.
						}
					}
				}
				if (leadToNoInput) {
					if (executionList.containsKey(layer)) {
						executionList.get(layer).add(service);
					} else {
						executionList.put(layer, new ArrayList<Service>());
						executionList.get(layer).add(service);
					}
				}
			}
		}

		executionList.forEach((layer, services) -> { // iterate through the executionList map, where Layers are keys and
														// an ArrayList of services is the value.
			for (Service s : services) {
				System.out.println(s.getUri().toASCIIString());
				layer.removeService(s);// for each layer, remove the redundant services.
			}
		});
		return executionList.size();
	}

	public void optimalSearch() {
		// This function will select out the optimal path when there is multiple paths
		// available
		OptimalPathSearcher searcher = new OptimalPathSearcher(this.newGraph);
		searcher.searchForTheOptimalPath();

	}

	public void visualizeGraph() {

	}

}
