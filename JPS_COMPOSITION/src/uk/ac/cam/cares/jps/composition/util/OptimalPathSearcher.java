package uk.ac.cam.cares.jps.composition.util;

import uk.ac.cam.cares.jps.composition.EngineModel.Graph;
import uk.ac.cam.cares.jps.composition.EngineModel.Layer;

public class OptimalPathSearcher {
	
	private Graph theGraph; 
	
	public OptimalPathSearcher(Graph theGraph) {
		this.setTheGraph(theGraph);// Waiting for the next iteration of upgrading... 		
	}
	
	public void searchForTheOptimalPath() {
		// Rule: Start from the last layer, get all the inputs, if for a certain input there is only one provider, label it as a 
		Layer lastLayer = theGraph.layers.get(theGraph.layers.size() -1);
 		
		
	}

	public void identifyTheSingleProviders() {
		// get all edges to the final services, identify the single providers, iterate on them
		
		
	}
	
	
	public Graph getTheGraph() {
		return theGraph;
	}

	public void setTheGraph(Graph theGraph) {
		this.theGraph = theGraph;
	}
}
