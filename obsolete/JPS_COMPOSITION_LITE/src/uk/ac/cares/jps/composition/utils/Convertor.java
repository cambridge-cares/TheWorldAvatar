package uk.ac.cares.jps.composition.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cares.jps.composition.compositionagent.CompositionResult;

public class Convertor {

	public static ArrayList<String> convert_JSONArray_to_ArrayList(JSONArray jArray) {
		ArrayList<String> listdata = new ArrayList<String>();
		if (jArray != null) {
			for (int i = 0; i < jArray.length(); i++) {
				listdata.add(jArray.getString(i));
			}
		}
		return listdata;
	}

	public static ArrayList<ArrayList<String>> convert_layers_in_JSON_to_ArrayList(JSONArray layers_in_JSON) {
		ArrayList<ArrayList<String>> layers = new ArrayList<ArrayList<String>>();
		for (int i = 0; i < layers_in_JSON.length(); i++) {
			JSONArray a_layer_in_JSON = layers_in_JSON.getJSONArray(i);
			layers.add(Convertor.convert_JSONArray_to_ArrayList(a_layer_in_JSON));
		}
		return layers;
	}

	public static Map<String, ArrayList<String>> convert_mapping_in_JSON_to_Map(JSONObject mapping_in_JSON) {

		Map<String, ArrayList<String>> result = new HashMap<String, ArrayList<String>>();
		Iterator<String> keys = mapping_in_JSON.keys();
		while (keys.hasNext()) {
			String key = keys.next();
			if (mapping_in_JSON.getJSONArray(key) instanceof JSONArray) {
				result.put(key, convert_JSONArray_to_ArrayList(mapping_in_JSON.getJSONArray(key)));
			}
		}

		return result;
	}

	// With the convenience of Jackson serialization tool, we can easily serialize
	// the composition result ...
	public static JSONObject serialize_composition_result(CompositionResult composition_result)
			throws JSONException, JsonProcessingException {

		ObjectMapper mapper = new ObjectMapper();
		JSONObject result = new JSONObject(
				mapper.writerWithDefaultPrettyPrinter().writeValueAsString(composition_result));
		return result;
	}

	public static CompositionResult deserialize_composition_result(JSONObject composition_result_in_JSON)
			throws JsonParseException, JsonMappingException, IOException {

		ObjectMapper mapper = new ObjectMapper();
		return mapper.readValue(composition_result_in_JSON.toString(), CompositionResult.class);
	}

	public static Map<String, String> deserializejsonobject(JSONObject map_in_json)
			throws JsonParseException, JsonMappingException, IOException {
		ObjectMapper mapper = new ObjectMapper();
		TypeReference<HashMap<String, String>> typeRef = new TypeReference<HashMap<String, String>>() {
		};
		Map<String, String> mapping = mapper.readValue(map_in_json.toString(), typeRef);
		return mapping;
	}
}
