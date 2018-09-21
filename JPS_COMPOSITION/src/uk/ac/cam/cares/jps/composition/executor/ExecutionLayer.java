package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;

public class ExecutionLayer {
	// an executionlayer contains a list of task and collect
	// the results returned by the task
	// a executionlayer should return the result

	public ArrayList<Task> taskList;
	public ArrayList<ExecutionPackage> resultList;

	public ExecutionLayer() {
		this.taskList = new ArrayList<Task>();
		this.resultList = new ArrayList<ExecutionPackage>();
	}

}
