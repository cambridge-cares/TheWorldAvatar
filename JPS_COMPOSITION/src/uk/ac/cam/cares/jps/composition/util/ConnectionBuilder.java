package uk.ac.cam.cares.jps.composition.util;

import java.util.ArrayList;

import uk.ac.cam.cares.jps.composition.EngineModel.Edge;
import uk.ac.cam.cares.jps.composition.EngineModel.Graph;
import uk.ac.cam.cares.jps.composition.EngineModel.Layer;
import uk.ac.cam.cares.jps.composition.ServiceModel.MessagePart;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class ConnectionBuilder {
	public void buildEdge(Graph theGraph) {
		// iterate through all the services here, for each 
		ArrayList<Layer> previousLayers = new ArrayList<Layer>();
		for (Layer layer: theGraph.layers) {
			previousLayers.add(layer);
			for (Service service: layer.getServices()) {
				// Compare the previous layers' outputs with this service's inputs, locate a match
				for (Layer previousLayer: previousLayers) {
					for(Service previousService: previousLayer.getServices()) {
						for(MessagePart previousOutput: previousService.getAllOutputs()) {
							for(MessagePart currentInput: service.getAllInputs()) {
								if(MatchingTool.compareURI(previousOutput.getModelReference(),currentInput.getModelReference()))
								{
									Edge newEdge = new Edge();
									newEdge.fromNode = previousService;
									newEdge.toNode = service;
									newEdge.fromOutput = previousOutput;
									newEdge.toInput = currentInput;
									theGraph.edges.add(newEdge);
									//previousService.addOutputEdge(newEdge);
								}
							}	
						}
					}
				}
			}
		}
	}
	
	
	
	
	
}
