package uk.ac.cam.cares.jps.composition.executor;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class ExecutorNew {

	public ArrayList<ExecutionLayer> executionChain;
	public Map<String,ExecutionPackage> executionPackageMap = new HashMap<String,ExecutionPackage>();  // The key is the http url
	public String myHost = "localhost";
	public int myPort = 8080;
	public JSONArray finalResult = new JSONArray();
	
	
	public ExecutorNew() {
		
	}
	
	public ExecutorNew(ArrayList<ExecutionLayer> executionChain) {
		this.executionChain = executionChain;
	}
	
	
	public String execute(JSONObject initialInput) throws JSONException {
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
							JSONObject result = new JSONObject(executeGet(builder));
							executeSingleTask(task, result, targetHttpUrlList, keysArray);
						}
						else {
							for (Entry<String, ExecutionPackage> entry : this.executionPackageMap.entrySet())
							{
								ExecutionPackage executionPackage = entry.getValue();
								if(executionPackage.readyToExecute && (executionPackage.targetHttpUrl.equalsIgnoreCase(task.httpUrl))) {
									URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
											.setPath(path)
											.setParameter("value", executionPackage.result.toString());
									JSONObject result = new JSONObject(executeGet(builder));
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
						this.finalResult.put(result);
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
	
	
	public void executeSingleTask(Task task, JSONObject result, ArrayList<String> targetHttpUrlList, ArrayList<ArrayList<String>> keysArray) throws JSONException {
		if(task.targetHttpUrl.size() == 0) {
			// The result is one of the final results ; 
			this.finalResult.put(result);
		}
	
		for(int i = 0; i < targetHttpUrlList.size(); i++) {
			String targetHttpUrl = targetHttpUrlList.get(i);
			ArrayList<String> keys = keysArray.get(i);
			if(!this.executionPackageMap.containsKey(targetHttpUrl)) {
				ExecutionPackage newExecutionPackage = new ExecutionPackage();
				newExecutionPackage.keys = keys;
				newExecutionPackage.targetHttpUrl = targetHttpUrl;
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
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
}
