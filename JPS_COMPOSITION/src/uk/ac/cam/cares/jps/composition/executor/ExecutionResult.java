package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;

import org.apache.jena.rdf.model.Model;

public class ExecutionResult {
	// it contains two components, the result of an execution and the index
	// array of the service that its going to pass

	public String result;
	public ArrayList<String> targetHttpUrl;
	public ExecutionResult() {
		this.targetHttpUrl = new ArrayList<String>();
	}

}
