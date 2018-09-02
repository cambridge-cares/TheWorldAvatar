package uk.ac.cam.cares.jps.composition.executor;

import java.net.URI;
import java.util.ArrayList;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
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
		System.out.println(initialInput);
		ArrayList<ExecutionResult> resultPool = new ArrayList<ExecutionResult>();
		int layerIdx = 0;
		for (ExecutionLayer layer : this.executionChain) {
			for (Task task : layer.taskList) {
				ExecutionResult result = new ExecutionResult();
				if (layerIdx == 0) {
					result.result = SendRequest.sendGet(task.httpUrl.replace("www.theworldavatar.com", "localhost:8080")
							+ "?value=" + new JSONObject(initialInput).toString().replaceAll("#", "@"));
					result.targetHttpUrl.addAll(task.targetHttpUrl);
				} else {
					String httpUrl = task.httpUrl;
					for (int i = 0; i < resultPool.size(); i++) {
						ExecutionResult previousResult = resultPool.get(i);
						for (int j = 0; j < previousResult.targetHttpUrl.size(); j++) {
							String previousTargetHttpUrl = previousResult.targetHttpUrl.get(j);
							if (previousTargetHttpUrl.contentEquals(httpUrl)) {
								String myHost = "localhost";
								int myPort = 8080;
								String path = previousTargetHttpUrl.replace("http://www.theworldavatar.com","");
															
								URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
										.setPath(path)
										.setParameter("value",previousResult.result.replaceAll("#", "@"));
								result.result =  executeGet(builder);										

								if (task.targetHttpUrl != null) {
									result.targetHttpUrl.addAll(task.targetHttpUrl);
								} else {
									return result.result;
								}
							}
						}
					}
				}
				resultPool.add(result);
			}
			layerIdx++;
		}
		return "Error";
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
