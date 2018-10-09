package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;

import org.json.JSONException;
import org.json.JSONObject;

public class ExecutionPackageCollection {

	public boolean readyToExecute = false;
	public JSONObject result;
	public ArrayList<ExecutionPackage> packageList;
	public String targetHttpUrl; 
	
	public ExecutionPackageCollection() {
		this.packageList = new ArrayList<ExecutionPackage>();
		this.result = new JSONObject();
	}
	
	
	public void appendNewPackage(ExecutionPackage newPackage) {
		this.packageList.add(newPackage);
		
		
		// Add a new package to the collection
		// Check the HTTP, merge results... 
	}
	
 	
	public void mergeResults() {
		
	}
	
	public void mergetMapping() {
		
	}
	
	public JSONObject appendNewResult(JSONObject newResult, ArrayList<String> keys) throws JSONException {

		boolean ready = true; 
		for(String key: keys) {
			if(newResult.has(key)) {
				System.out.println("--------------------");
				System.out.println("Keys : " + keys.toString());
				System.out.println("appending key: " + key);
				System.out.println("value to append: " + newResult.toString());
				System.out.println("--------------------");
				this.result.put(key, newResult.get(key));
			}
			
			if(!this.result.has(key)) {
				ready = false;
			}
		}
		
		this.readyToExecute = ready; // Now the ExecutionPackage is ready. 
		return this.result;
	}
	
	
}
