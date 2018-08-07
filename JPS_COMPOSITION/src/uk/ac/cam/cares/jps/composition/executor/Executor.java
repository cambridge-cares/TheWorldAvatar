package uk.ac.cam.cares.jps.composition.executor;

import java.util.ArrayList;

import org.json.JSONObject;

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
					// find the target url first
					String httpUrl = task.httpUrl;
					for (int i = 0; i < resultPool.size(); i++) {
						ExecutionResult previousResult = resultPool.get(i);
						for (int j = 0; j < previousResult.targetHttpUrl.size(); j++) {
							String previousTargetHttpUrl = previousResult.targetHttpUrl.get(j);
							if (previousTargetHttpUrl.contentEquals(httpUrl)) {
								// execute
								result.result = SendRequest.sendGet(
										previousTargetHttpUrl.replaceAll("www.theworldavatar.com", "localhost:8080")
												+ "?value=" + previousResult.result);
								System.out.println(result.result);
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

}
