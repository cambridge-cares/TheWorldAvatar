package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;

public class ExecutionPackage {
	// it contains two components, the result of an execution and the index
	// array of the service that its going to pass

	public JSONObject result;
	public String targetHttpUrl;
	public ArrayList<String> keys;
	public boolean readyToExecute = false;
	public Map<String, Map<String, String>> nameMappingList;
	
	
	public ExecutionPackage() {
		this.keys = new ArrayList<String>();
		this.result = new JSONObject();
		this.nameMappingList = new HashMap<String, Map<String, String>>();
	}
	
	public void appendNewResult(JSONObject newResult) throws JSONException {

		boolean ready = true; 
		for(String key: keys) {
			if(newResult.has(key)) {
				this.result.put(key, newResult.get(key));
			}
			
			if(!this.result.has(key)) {
				ready = false;
			}
		}
		
		this.readyToExecute = ready; // Now the ExecutionPackage is ready. 
	}
	
	

}
