package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.composition.servicemodel.MessagePart;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class ExecutorProcessor {

	public JSONObject compositeService;
	public JSONArray layers;
	public ArrayList<Task> taskList;
	public Map<String, ArrayList<String>> resultPool;
	public ArrayList<String> eliminationList;
	public JSONObject map;
 	public ServiceDiscovery discovery;
	
	public ExecutorProcessor(JSONObject compositionResult, ArrayList<String> eliminationList) throws Exception {
		this.compositeService = compositionResult;
		this.layers = this.compositeService.getJSONArray("layers");
		this.taskList = new ArrayList<Task>();
		this.eliminationList = eliminationList;
		this.resultPool = new HashMap<String, ArrayList<String>>();
		this.map = new JSONObject();
	
		// Initiate a new ServiceDiscoveryPool
		this.discovery = ServiceDiscovery.getInstance();
	}
	
	public ExecutorProcessor(JSONObject compositionResult) throws Exception {
		this.compositeService = compositionResult;
		this.layers = this.compositeService.getJSONArray("layers");
		this.taskList = new ArrayList<Task>();
		
		this.eliminationList = new ArrayList<String>();
		JSONArray tempList;
		if(!this.compositeService.has("eliminationList")) {
			tempList = new JSONArray();
		}
		else {
			tempList = this.compositeService.getJSONArray("eliminationList");
		}
		for (int idx = 0; idx < tempList.length(); idx++) {
			this.eliminationList.add(tempList.getString(idx));
		}
		
		this.resultPool = new HashMap<String, ArrayList<String>>();
		this.map = new JSONObject();
		this.discovery = ServiceDiscovery.getInstance();

	}
	
	

	public ExecutionLayer LayerAnalyzer(JSONObject layer) throws JSONException {
		ExecutionLayer newExecutionLayer = new ExecutionLayer();
		int layerOffSet = 0;
		for (int i = 0; i < layer.getJSONArray("services").length(); i++) {
			JSONObject service = layer.getJSONArray("services").getJSONObject(i);
			Task newTask = ServiceAnalyzer(service, layerOffSet);
			if (newTask != null) {
				newExecutionLayer.taskList.add(newTask);
			}
		}
		return newExecutionLayer;
	}

	public Task ServiceAnalyzer(JSONObject service, int layerOffSet) throws JSONException {
		// get the httpUrl of the service
		// get the index of its target service in the next layer...
		JSONObject operation = service.getJSONArray("operations").getJSONObject(0);
		String httpUrl = operation.getString("httpUrl");
		// httpUrl is the upstream agent which produces the outputs 
		Service upstreamService = this.discovery.getServiceFromHttpUrl(httpUrl); 
					
		Task task = new Task(httpUrl);
		task.outputIndexArray = getIndex(service).get(0);
		task.inputIndexArray = getIndex(service).get(1);
		if (!this.eliminationList.contains(service.getString("uri"))) {
			ArrayList<String> nextHttpUrlList = new ArrayList<String>();
			for (int[] array : task.outputIndexArray) {
				if (this.map.has(array[0] + "_" + array[1])) {
					int newLayerIdx = this.map.getInt(array[0] + "_" + array[1]);
					int newServiceIdx = 0;
					array = new int[] { newLayerIdx, newServiceIdx };
					nextHttpUrlList.add(getHttpUrl(array));
					layerOffSet++;
				} else {
					int newLayerIdx = array[0];
					int newServiceIdx = array[1] - layerOffSet;
					array = new int[] { newLayerIdx, newServiceIdx };
					nextHttpUrlList.add(getHttpUrl(array));
				}
			}
			task.targetHttpUrl = nextHttpUrlList;
			for(int i = 0; i < nextHttpUrlList.size(); i++) {
				task.keysArray.add(getAllOutputKeysFromTargetHTTP(httpUrl));
			}
			
			// Find the mapping of 
			
			// ======================================= A task has multiple target http url =====================
			// 
			for(String url : task.targetHttpUrl) {
			    Service downstreamService = this.discovery.getServiceFromHttpUrl(url);
				Map<String, Map<String,String>> listOfMaps = new HashMap<String, Map<String,String>>();
				for(MessagePart input : downstreamService.getAllInputs()) {
					Map<String,Map<String,String>> nameToNameMap = input.getMapOfUpstreamNamesAndDownstreamNames(upstreamService);
					// This is the nested leaved of a certain input ...
					if(nameToNameMap!=null) {
						listOfMaps.putAll(nameToNameMap);
					}
				}
				
				task.httpToNameMapping.put(url, listOfMaps);

			}
			//=====================================================================================================
			
	
			
			
			return task;
		} else {
			return null;
		}
	}

	public ArrayList<ArrayList<int[]>> getIndex(JSONObject service) throws JSONException {
		ArrayList<int[]> resultOutput = new ArrayList<int[]>();
		ArrayList<int[]> resultInput = new ArrayList<int[]>();

		ArrayList<ArrayList<int[]>> result = new ArrayList<ArrayList<int[]>>();
		JSONArray outputs = service.getJSONArray("operations").getJSONObject(0).getJSONArray("outputs");
		JSONArray inputs = service.getJSONArray("operations").getJSONObject(0).getJSONArray("inputs");

		for (int i = 0; i < outputs.length(); i++) {
			JSONObject output = outputs.getJSONObject(i);
			JSONArray OutputMandatoryParts = output.getJSONArray("mandatoryParts");
			for (int j = 0; j < OutputMandatoryParts.length(); j++) {
				if (OutputMandatoryParts.getJSONObject(j).has("outputEdges")) {
					JSONArray outputEdges = OutputMandatoryParts.getJSONObject(j).getJSONArray("outputEdges");
					for (int k = 0; k < outputEdges.length(); k++) {
						JSONArray toInput = outputEdges.getJSONObject(k).getJSONArray("toInput");
						int[] index = { toInput.getInt(0), toInput.getInt(1) };
						resultOutput.add(index);
					}
				}
			}
		}

		for (int l = 0; l < inputs.length(); l++) {
			JSONObject input = inputs.getJSONObject(l);
			JSONArray InputMandatoryParts = input.getJSONArray("mandatoryParts");
			for (int m = 0; m < InputMandatoryParts.length(); m++) {
				if (InputMandatoryParts.getJSONObject(m).has("inputEdges")) {
					JSONArray inputEdges = InputMandatoryParts.getJSONObject(m).getJSONArray("inputEdges");
					for (int n = 0; n < inputEdges.length(); n++) {
						JSONArray fromOutput = inputEdges.getJSONObject(n).getJSONArray("fromOutput");
						int[] indexOutput = { fromOutput.getInt(0), fromOutput.getInt(1) };
						resultInput.add(indexOutput);
					}
				}
			}
		}

		result.add(resultOutput);
		result.add(resultInput);
		return result;
	}
	
	public ArrayList<String> getAllInputKeysFromTargetHTTP(String targetHttp) throws JSONException {
		JSONArray layers = this.compositeService.getJSONArray("layers");
		ArrayList<String> result = new ArrayList<String>();
		for (int i = 0; i < layers.length(); i++) {
			JSONObject layer = layers.getJSONObject(i);
			JSONArray services = layer.getJSONArray("services");
			for(int j = 0; j < services.length(); j++) {
				JSONObject service = services.getJSONObject(j);
				JSONObject operation = service.getJSONArray("operations").getJSONObject(0);
				String http = operation.getString("httpUrl");

				if(targetHttp.equalsIgnoreCase(http)) {
					JSONArray inputs = operation.getJSONArray("inputs");
					for (int l = 0; l < inputs.length(); l++) {
						JSONObject input = inputs.getJSONObject(l);
						JSONArray InputMandatoryParts = input.getJSONArray("mandatoryParts");
						for (int m = 0; m < InputMandatoryParts.length(); m++) {
							if (InputMandatoryParts.getJSONObject(m).has("name")) {
								String name = InputMandatoryParts.getJSONObject(m).getString("name");
								result.add(name);
							}
						}
					}
				}
			}
		}
		return result;
	}
	
	
	public ArrayList<String> getAllOutputKeysFromTargetHTTP(String targetHttp) throws JSONException {
		JSONArray layers = this.compositeService.getJSONArray("layers");
		ArrayList<String> result = new ArrayList<String>();
		for (int i = 0; i < layers.length(); i++) {
			JSONObject layer = layers.getJSONObject(i);
			JSONArray services = layer.getJSONArray("services");
			for(int j = 0; j < services.length(); j++) {
				JSONObject service = services.getJSONObject(j);
				JSONObject operation = service.getJSONArray("operations").getJSONObject(0);
				String http = operation.getString("httpUrl");

				if(targetHttp.equalsIgnoreCase(http)) {
					JSONArray inputs = operation.getJSONArray("outputs");
					for (int l = 0; l < inputs.length(); l++) {
						JSONObject input = inputs.getJSONObject(l);
						JSONArray InputMandatoryParts = input.getJSONArray("mandatoryParts");
						for (int m = 0; m < InputMandatoryParts.length(); m++) {
							if (InputMandatoryParts.getJSONObject(m).has("name")) {
								String name = InputMandatoryParts.getJSONObject(m).getString("name");
								result.add(name);
							}
						}
					}
				}
			}
		}
		return result;
	}
	
	public ArrayList<String> getAllTypesFromTargetHTTP(String targetHttp) throws JSONException {
		JSONArray layers = this.compositeService.getJSONArray("layers");
		ArrayList<String> result = new ArrayList<String>();
		for (int i = 0; i < layers.length(); i++) {
			JSONObject layer = layers.getJSONObject(i);
			JSONArray services = layer.getJSONArray("services");
			for(int j = 0; j < services.length(); j++) {
				JSONObject service = services.getJSONObject(j);
				JSONObject operation = service.getJSONArray("operations").getJSONObject(0);
				String http = operation.getString("httpUrl");

				if(targetHttp.equalsIgnoreCase(http)) {
					JSONArray inputs = operation.getJSONArray("inputs");
					for (int l = 0; l < inputs.length(); l++) {
						JSONObject input = inputs.getJSONObject(l);
						JSONArray InputMandatoryParts = input.getJSONArray("mandatoryParts");
						for (int m = 0; m < InputMandatoryParts.length(); m++) {
							if (InputMandatoryParts.getJSONObject(m).has("modelReference")) {
								String type = InputMandatoryParts.getJSONObject(m).getString("modelReference");
								result.add(type);
							}
						}
					}
				}
			}
		}
		return result;
	}

	public String getKeyFromType(String type) throws JSONException {
		JSONArray layers = this.compositeService.getJSONArray("layers");
		String result = null;
		for (int i = 0; i < layers.length(); i++) {
			JSONObject layer = layers.getJSONObject(i);
			JSONArray services = layer.getJSONArray("services");
			for(int j = 0; j < services.length(); j++) {
				JSONObject service = services.getJSONObject(j);
				JSONObject operation = service.getJSONArray("operations").getJSONObject(0);
				JSONArray inputs = operation.getJSONArray("inputs");
				for (int l = 0; l < inputs.length(); l++) {
					JSONObject input = inputs.getJSONObject(l);
					JSONArray InputMandatoryParts = input.getJSONArray("mandatoryParts");
					for (int m = 0; m < InputMandatoryParts.length(); m++) {
						if (InputMandatoryParts.getJSONObject(m).has("modelReference")) {
							String _type = InputMandatoryParts.getJSONObject(m).getString("modelReference");
							if(_type.equalsIgnoreCase(type)) {
								String name = InputMandatoryParts.getJSONObject(m).getString("name");
								result = name;
							}
						}
					}
				}
				
			}
		}
		return result;
	}
	
	
	
	
	public String getHttpUrl(int[] index) throws JSONException {
		JSONObject layer = this.layers.getJSONObject(index[0]);
		JSONObject service = layer.getJSONArray("services").getJSONObject(index[1]);
		JSONArray operation = service.getJSONArray("operations");
		return operation.getJSONObject(0).getString("httpUrl");
	}

	public ArrayList<ExecutionLayer> generateExecutionChain() throws JSONException {

		ArrayList<ExecutionLayer> executionChain = new ArrayList<ExecutionLayer>();
		if (this.compositeService.has("updatesMap")) {
			this.map = this.compositeService.getJSONObject("updatesMap");
		}
		JSONArray layers = this.compositeService.getJSONArray("layers");

		for (int i = 0; i < layers.length(); i++) {
			JSONObject layer = layers.getJSONObject(i);
			ExecutionLayer newExecutionLayer = LayerAnalyzer(layer);
			executionChain.add(newExecutionLayer);
		}
		return executionChain;
	}

}
