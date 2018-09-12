package uk.ac.cam.cares.jps.composition.executor;

import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.ArrayList;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.composition.util.SendRequest;

public class Executor {

	public ArrayList<ExecutionLayer> executionChain;

	public Executor() {

	}

	public Executor(ArrayList<ExecutionLayer> executionChain) {
		this.executionChain = executionChain;

	}

	public String execute(String initialInput) throws Exception {
		
		Model initInputsModel = ModelFactory.createDefaultModel();
		System.out.println("======= Init Input ==========");
		System.out.println(initialInput);
		
 		RDFDataMgr.read(initInputsModel, new ByteArrayInputStream(initialInput.getBytes("UTF-8")), Lang.RDFJSON);
		ArrayList<ExecutionResult> resultPool = new ArrayList<ExecutionResult>(); // This pool stores result from each execution... 
		ArrayList<ExecutionResult> finalResultPool = new ArrayList<ExecutionResult>(); // Only final results are stored in this pool .. 
		
		
		Model dataBundle = ModelFactory.createDefaultModel();
		ArrayList<String> dataBundleStringList = new ArrayList<String>();

		
		int layerIdx = 0;
 		for (ExecutionLayer layer : this.executionChain) {
			for (Task task : layer.taskList) {
				Boolean hitTheEnd = false;
				ExecutionResult result = new ExecutionResult();
				if (layerIdx == 0) {

						String result1 = SendRequest.sendGet(task.httpUrl.replace("www.theworldavatar.com", "localhost:8080")
							+ "?value=" + new JSONObject(initialInput).toString().replace("#", "@"));
						result.result = result1;
						if(task.targetHttpUrl!=null) {
							result.targetHttpUrl.addAll(task.targetHttpUrl);
							resultPool.add(result);
							// TODO: IF the result is part of the output pool, Add it to the finalResult Pool 							
						}
						else {
							finalResultPool.add(result);
						}
						
						
						
				} else {
					// Iterate to a new task ... 

					String httpUrl = task.httpUrl; 
					// Find all the results return from previous layers, if the result's target url matches the current task's url 
				
					for (int i = 0; i < resultPool.size(); i++) { // Iterate through all the results collected
						ExecutionResult previousResult = resultPool.get(i);
						Boolean matchTheCurrentTask = false;
						for (int j = 0; j < previousResult.targetHttpUrl.size(); j++) {
							String previousTargetHttpUrl = previousResult.targetHttpUrl.get(j);
							if (previousTargetHttpUrl.contentEquals(httpUrl)) {
								// Match, add the result to dataBundle 
								if (task.targetHttpUrl != null) { // Not the end 
									matchTheCurrentTask = true; 
									result.targetHttpUrl.addAll(task.targetHttpUrl);
									
								} else {
									hitTheEnd = true;
								}
							}
						}
						
						if(matchTheCurrentTask) {
							// Add the result to current dataBundle...
							dataBundleStringList.add(previousResult.result);

						}
							
					}
					String myHost = "localhost";
					int myPort = 8080;
					String path = httpUrl.replace("http://www.theworldavatar.com","");

					for(String input : dataBundleStringList) {
						Model inputModel = ModelFactory.createDefaultModel();
						RDFDataMgr.read(inputModel, new ByteArrayInputStream(input.getBytes("UTF-8")), Lang.RDFJSON);
						System.out.println("=======================" + httpUrl);
						System.out.println(input);
						System.out.println("=======================" + httpUrl);
						dataBundle.add(inputModel);
					}
					
					Boolean takesInitialInputs = false;
					for(int[] idxs : task.inputIndexArray)
					{
						if (idxs[0] == -1) {
							takesInitialInputs = true;
						}
					}
					
					if(takesInitialInputs) {
						dataBundle.add(initInputsModel);
					}
					
					StringWriter out = new StringWriter();
					RDFDataMgr.write(out, dataBundle, RDFFormat.RDFJSON);
					URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
							.setPath(path)
							.setParameter("value",out.toString().replace("#","@"));
					String result2 =  executeGet(builder);		
					result.result = result2;
					if(hitTheEnd) {
						finalResultPool.add(result);
					} 
					
				}
				resultPool.add(result);
				dataBundleStringList.add(result.result);
			}
			layerIdx++;
		}
  
		return buildAndSerializeResultModel(finalResultPool);
	}

	
	
	
	

	
	public String buildAndSerializeResultModel(ArrayList<ExecutionResult> resultPool) {
		
		Model resultBundle = ModelFactory.createDefaultModel();
		for(ExecutionResult result : resultPool) {
			// Convert it to rdf Model
			Model model = ModelFactory.createDefaultModel();
			try {
				RDFDataMgr.read(model, new ByteArrayInputStream(result.result.getBytes("UTF-8")), Lang.RDFJSON);
				resultBundle.add(model);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
		}
		
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, resultBundle, Lang.RDFJSON);
		return out.toString().replace("$", "#").replace("@", "#");
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
