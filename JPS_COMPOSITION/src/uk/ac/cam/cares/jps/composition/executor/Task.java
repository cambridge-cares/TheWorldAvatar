package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(value = { "outputIndexArray" })

public class Task {

	public String httpUrl;
	public ArrayList<String> targetHttpUrl;
	public ArrayList<int[]> outputIndexArray;
	public ArrayList<int[]> inputIndexArray;
	public ExecutionResult result;
	public Task() {

	}

	public Task(String httpUrl) {
		this.httpUrl = httpUrl;
		this.outputIndexArray = new ArrayList<int[]>();
		this.inputIndexArray = new ArrayList<int[]>();
		this.targetHttpUrl = new ArrayList<String>();
	}
}
