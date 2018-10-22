package uk.ac.cam.cares.jps.composition.executor;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class ExecutorNew {
	
	Logger logger = LoggerFactory.getLogger(ExecutorNew.class);

	public ArrayList<ExecutionLayer> executionChain;
	public Map<String,ExecutionPackage> executionPackageMap = new HashMap<String,ExecutionPackage>();  // The key is the http url
	public String myHost = "localhost";
	public int myPort = 8080;
	public JSONObject finalResult = new JSONObject();
	public JSONObject initialInput;
	
	public ExecutorNew() {
		
	}
	
	public ExecutorNew(ArrayList<ExecutionLayer> executionChain) {
		this.executionChain = executionChain;
	}

	public String execute(JSONObject initialInput) throws JSONException {
		this.initialInput = initialInput;
		int agentType = checkCompositeAgentType();
		switch(agentType) {
			case -1 : {
				System.out.println("Normal execution");
				int layerCounter = 0; 
				for(ExecutionLayer layer : this.executionChain) {
					for(Task task : layer.taskList)	{  
						String executionHttpUrl = task.httpUrl;
						ArrayList<String> targetHttpUrlList = task.targetHttpUrl;
						ArrayList<ArrayList<String>> keysArray = task.keysArray;
						String path = executionHttpUrl.replace("http://www.theworldavatar.com","");
						if(layerCounter == 0) {
							URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
									.setPath(path)
									.setParameter("value", initialInput.toString());
							String resultString = executeGet(builder);
							JSONObject result = new JSONObject(resultString);
							//System.out.println("\n\tResult: \n" + result);
							executeSingleTask(task, result, targetHttpUrlList, keysArray);
						}
						else {
							for (Entry<String, ExecutionPackage> entry : this.executionPackageMap.entrySet())
							{
								// Iterate through the executionPackageMap. 
								// Each executionPackage within the map contains a targetHttpUrl
								// If the targetHttpUrl 
								ExecutionPackage executionPackage = entry.getValue();
								if(executionPackage.targetHttpUrl.equalsIgnoreCase(task.httpUrl)) {
									for(String key: executionPackage.keys) {
										Map<String, String> nameMapping = executionPackage.nameMappingList.get(key);
										logger.debug("nameMapping: " +  nameMapping);
										executionPackage.result = replaceKeysInJSON(executionPackage.result,nameMapping);
										logger.debug("executionPackage: " +  executionPackage.result.toString());

									}
									URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
											.setPath(path)
											.setParameter("value", executionPackage.result.toString());
									String resultInString = executeGet(builder);	 
									JSONObject result = new JSONObject(resultInString);
									executeSingleTask(task, result, targetHttpUrlList, keysArray);
								}
							}	
						}
					}
					layerCounter++;
				}
				break;
			}
			
			case 0 : {
				System.out.println("Single Agent");
				for(ExecutionLayer layer : this.executionChain) {
					for(Task task : layer.taskList)	{ 
						String executionHttpUrl = task.httpUrl;
						String path = executionHttpUrl.replace("http://www.theworldavatar.com","");
						URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
								.setPath(path)
								.setParameter("value", initialInput.toString());
						JSONObject result = new JSONObject(executeGet(builder));
						appendNewResult(result);
						//this.finalResult.put(result);
					}
				}
				break;
			}
			
			case 1 : {
				System.out.println("Single Layer");
				break;
			}
		}
		return this.finalResult.toString();
	}
	
 
	public JSONObject replaceKeysInJSON(JSONObject jsonObj, Map<String,String> nameMapping) throws JSONException
	{ 
		String jsonInString = jsonObj.toString();
		// Collect keys first
		ArrayList<String> keys = new ArrayList<String>();
		
		if(nameMapping!=null) {
		
 		for (Entry<String, String> entry : nameMapping.entrySet()) {
			keys.add(entry.getKey());
		}		
		Collections.sort(keys);		
		for(String key: keys) {
			jsonInString = jsonInString.replace(key, nameMapping.get(key));
		}
		
		return new JSONObject(jsonInString);
		}
		else {
			return jsonObj;
		}
	}
	 
	public void executeSingleTask(Task task, JSONObject result, ArrayList<String> targetHttpUrlList, ArrayList<ArrayList<String>> keysArray) throws JSONException {
		if(task.targetHttpUrl.size() == 0) {
			// The result is one of the final results ; 
			//this.finalResult.put(result);
			appendNewResult(result);
			
		}
		
		Map<String,Map<String, Map<String, String>>> NameMappingMap = task.httpToNameMapping;
		// Get all the name mappings from the http first 
		
		for(int i = 0; i < targetHttpUrlList.size(); i++) {
			String targetHttpUrl = targetHttpUrlList.get(i);
			if(!this.executionPackageMap.containsKey(targetHttpUrl)) {
				ArrayList<String> keys = keysArray.get(i);
				ExecutionPackage newExecutionPackage = new ExecutionPackage();
				newExecutionPackage.keys = keys;
				newExecutionPackage.nameMappingList = NameMappingMap.get(targetHttpUrl); 
				newExecutionPackage.targetHttpUrl = targetHttpUrl; // A single http address, assigned with and arrayList of name mapping... 
				newExecutionPackage.appendNewResult(this.initialInput);
				newExecutionPackage.appendNewResult(result);
				this.executionPackageMap.put(targetHttpUrl, newExecutionPackage);
			}
			else {
				ExecutionPackage newExecutionPackage = this.executionPackageMap.get(targetHttpUrl);
				newExecutionPackage.appendNewResult(result);
			} 
		}
	}
	
	
	public int checkCompositeAgentType() {
		// 1. Check whether it is a single agent situation
		int layerNumber = executionChain.size();
		if(layerNumber <= 1) {
			ExecutionLayer firstLayer = executionChain.get(0);
			int agentNumber = firstLayer.taskList.size();
			if(agentNumber == 1) {
				return(0);
			}
			else {
				return(1);
			}
		}
		else {
			return(-1);
		}
	}
	
	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			
			String s = uri.toString();
			String path = s.substring(0, s.indexOf('?'));
			logger.info("requesting " + path + "?" + uri.getQuery());
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
	
	public void appendNewResult(JSONObject newResult) throws JSONException {

		 
		Iterator<String> keys = newResult.keys();
		while(keys.hasNext()) {
			String key = (String) keys.next();
			this.finalResult.put(key, newResult.get(key));
		}
		

	}
}
