package uk.ac.cam.cares.jps.composition.util;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.composition.EngineModel.Branch;
import uk.ac.cam.cares.jps.composition.EngineModel.Graph;
import uk.ac.cam.cares.jps.composition.EngineModel.SubTrack;
import uk.ac.cam.cares.jps.composition.executor.ExecutorNew;
import uk.ac.cam.cares.jps.composition.ServiceModel.Service;

public class FormatTranslator {

	public static Service convertJSONTOJavaClass(String agentInJSONString)
			throws URISyntaxException, JsonParseException, JsonMappingException, IOException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return mapper.readValue(agentInJSONString, Service.class);
	}

	public static ExecutorNew convertJSONTOExecutor(String executorInJSONString)
			throws URISyntaxException, JsonParseException, JsonMappingException, IOException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return mapper.readValue(executorInJSONString, ExecutorNew.class);
	}

	public static Graph convertGraphJSONTOJavaClass(String graphInJSONString)
			throws URISyntaxException, JsonParseException, JsonMappingException, IOException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return mapper.readValue(graphInJSONString, Graph.class);
	}

	public static JSONObject convertJavaClassTOJSON(Service agentInJava) throws JsonProcessingException, JSONException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return new JSONObject(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(agentInJava));
	}

	public static JSONObject convertGraphJavaClassTOJSON(Graph graphInJava)
			throws JsonProcessingException, JSONException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return new JSONObject(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(graphInJava));
	}

	public static JSONObject convertBranchJavaClassTOJSON(Branch branchInJava)
			throws JsonProcessingException, JSONException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return new JSONObject(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(branchInJava));
	}

	public static JSONObject convertSubTrackJavaClassTOJSON(SubTrack subTrackInJava)
			throws JsonProcessingException, JSONException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return new JSONObject(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(subTrackInJava));
	}

	public static JSONObject convertExectorToJSON(ExecutorNew obj) throws JsonProcessingException, JSONException {
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		return new JSONObject(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(obj));
	}
	
	 
}