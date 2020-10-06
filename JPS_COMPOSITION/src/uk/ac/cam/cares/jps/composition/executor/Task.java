package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(value = { "outputIndexArray" })

public class Task {

	public String httpUrl;
	public ArrayList<String> targetHttpUrl;
	public ArrayList<int[]> outputIndexArray;
	public ArrayList<int[]> inputIndexArray;
	public ArrayList<ArrayList<String>> keysArray; // For each targetHttpUrl, there is an array of keys, indicating the keys of the result. 
	public ArrayList<ArrayList<String>> typesArray; // Now we add types along with the keysArray
	
	public Map<String,Map<String,Map<String,String>>> httpToNameMapping;
	
	
	public Task() {

	}

	public Task(String httpUrl) {
		this.httpUrl = httpUrl;
		this.outputIndexArray = new ArrayList<int[]>();
		this.inputIndexArray = new ArrayList<int[]>();
		this.targetHttpUrl = new ArrayList<String>();
		this.keysArray = new ArrayList<ArrayList<String>>();
		this.typesArray = new ArrayList<ArrayList<String>>();
		this.httpToNameMapping = new HashMap<String, Map<String,Map<String,String>>>();

	}
}
