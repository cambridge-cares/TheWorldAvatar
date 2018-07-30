package uk.ac.cam.cares.jps.composition.util;

import uk.ac.cam.cares.jps.composition.enginemodel.Graph;

public class OptimalPathSearcher {
	
	private Graph theGraph; 
	
	public OptimalPathSearcher(Graph theGraph) {
		this.setTheGraph(theGraph);// Waiting for the next iteration of upgrading... 		
	}
	
	public void searchForTheOptimalPath() {
		// The graph reaching this stage contains no redundant agents
		// Implement a path searching algorithm, state all the avaliable paths
		// composIT has an algorithm ... 
		
		
	}

	public Graph getTheGraph() {
		return theGraph;
	}

	public void setTheGraph(Graph theGraph) {
		this.theGraph = theGraph;
	}
}
