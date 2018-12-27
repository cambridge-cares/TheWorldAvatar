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
		for (Layer layer : theGraph.layers) {
			previousLayers.add(layer);
			int currentServiceIndex = 0;
			for (Service service : layer.getServices()) {
				currentServiceIndex++;
				// Compare the previous layers' outputs with this service's inputs, locate a
				// match
				for (Layer previousLayer : previousLayers) {
					int previousServiceIndex = 0;
					for (Service previousService : previousLayer.getServices()) {
						previousServiceIndex++;
						int previousInputIndex = 0;
						for (MessagePart previousOutput : previousService.getAllOutputs()) {
							previousInputIndex++;
							int currentInputIndex = 0;
							for (MessagePart currentInput : service.getAllInputs()) {
								currentInputIndex++;
								if (MatchingTool.compareURI(previousOutput.getType(),
										currentInput.getType())) {
									Edge newEdge = new Edge();
									int[] fromOutputIdx = new int[3];
									int[] toInputIdx = new int[3];
									fromOutputIdx[0] = previousLayer.getIndex() - 1;// layer index of an agent in
									fromOutputIdx[1] = previousServiceIndex - 1;
									fromOutputIdx[2] = previousInputIndex - 1;
																		
									// TODO: Get the key from the 
									
									toInputIdx[0] = layer.getIndex() - 1;
									toInputIdx[1] = currentServiceIndex - 1;
									toInputIdx[2] = currentInputIndex - 1;
									newEdge.fromOutput = fromOutputIdx;
									newEdge.toInput = toInputIdx;
									theGraph.edges.add(newEdge);
									if (previousLayer.getIndex() == layer.getIndex()) {
										service.moveToNextLayer = true;
									}

								}

								for (MessagePart initInput : theGraph.initialInputs) {
									if (MatchingTool.compareURI(initInput.getType(),
											currentInput.getType())) {
										Edge newEdge = new Edge();
										int[] toInputIdx = new int[3];
										toInputIdx[0] = layer.getIndex() - 1;
										toInputIdx[1] = currentServiceIndex - 1;
										toInputIdx[2] = currentInputIndex - 1;
										int[] fromOutputIdx = new int[3];
										fromOutputIdx[0] = -1;
										newEdge.fromOutput = fromOutputIdx;
										newEdge.toInput = toInputIdx;
										theGraph.edges.add(newEdge);
									}
								}
							}
						}
					}
				}
			}

		}
	}

	public void connectEdges(Graph theGraph) {
		for (Edge edge : theGraph.edges) {
			int[] fromOutputIdx = edge.fromOutput;
			int[] toInputIdx = edge.toInput;

			if (fromOutputIdx != null) {
				if (fromOutputIdx[0] != -1) {
					theGraph.layers.get(fromOutputIdx[0]).getServices().get(fromOutputIdx[1]).getAllOutputs()
							.get(fromOutputIdx[2]).outputEdges.add(edge);
				}

			}
			theGraph.layers.get(toInputIdx[0]).getServices().get(toInputIdx[1]).getAllInputs()
					.get(toInputIdx[2]).inputEdges.add(edge);

		}
	}

	public void rearrangeEdges(Graph theGraph) {
		int offset = 0;
		boolean flag = false;
		for (int layerIdx = 0; layerIdx < theGraph.layers.size(); layerIdx++) {
			for (int serviceIdx = 0; serviceIdx < theGraph.layers.get(layerIdx).getServices().size(); serviceIdx++) {
				if (theGraph.layers.get(layerIdx).getServices().get(serviceIdx).moveToNextLayer) {
					Service theService = new Service();
					theService = theGraph.layers.get(layerIdx).getServices().get(serviceIdx);
					offset++;
					flag = true;
					Layer newLayer = new Layer(layerIdx + 1);
					theGraph.layers.get(layerIdx)
							.removeService(theGraph.layers.get(layerIdx).getServices().get(serviceIdx));
					theService.moveToNextLayer = false;
					newLayer.addService(theService);
					theGraph.layers.add(layerIdx + 1, newLayer);
					int newLayerIndex = theGraph.layers.indexOf(newLayer);
					theGraph.updatesMap.put(layerIdx + "_" + serviceIdx, newLayerIndex);

					// get the index and update the edge info ...
				}
			}
			if (flag) {
				theGraph.layers.get(layerIdx).setIndex(layerIdx + offset);
			}
		}
	}

}