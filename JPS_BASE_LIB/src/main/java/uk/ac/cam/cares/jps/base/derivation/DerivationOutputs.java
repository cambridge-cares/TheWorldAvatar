package uk.ac.cam.cares.jps.base.derivation;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class DerivationOutputs {
	public static final String RETRIEVED_INPUTS_TIMESTAMP_KEY = "retrievedInputsAt";
	private Map<String, List<String>> outputs;
	private long retrievedInputsAt;

	public DerivationOutputs(JSONObject mappedInstances) {
		Map<String, List<String>> map = new HashMap<>();
		Iterator<String> keys = mappedInstances.keys();
		while (keys.hasNext()) {
			String key = keys.next();
			JSONArray arr = mappedInstances.getJSONArray(key);
			map.put(key, toList(arr));
		}
		this.outputs = map;
	}

	public DerivationOutputs(String rdfType, List<String> iris) {
		this.outputs = new HashMap<>();
		this.outputs.put(rdfType, iris);
	}

	public DerivationOutputs(String rdfType, String iri) {
		this.outputs = new HashMap<>();
		this.outputs.put(rdfType, Arrays.asList(iri));
	}

	public DerivationOutputs(Map<String, List<String>> outputs) {
		this.outputs = outputs;
	}

	public void setRetrievedInputsAt(long timestamp) {
		this.retrievedInputsAt = timestamp;
	}

	public long getRetrievedInputsAt() {
		return this.retrievedInputsAt;
	}

	public void addToOutputs(String rdfType, List<String> iris) {
		if (!this.outputs.containsKey(rdfType)) {
			this.outputs.put(rdfType, iris);
		} else {
			this.outputs.get(rdfType).addAll(iris);
		}
	}

	public void addToOutputs(String rdfType, String iri) {
		this.addToOutputs(rdfType, Arrays.asList(iri));
	}

	public Map<String, List<String>> getOutputs() {
		return this.outputs;
	}

	private static List<String> toList(JSONArray arr) throws JSONException {
		return IntStream.range(0, arr.length()).mapToObj(i -> arr.getString(i)).collect(Collectors.toList());
	}

	public List<String> getNewDerivedIRI() {
		return this.getOutputs().values().stream().flatMap(i -> i.stream()).collect(Collectors.toList());
	}

	public JSONObject toJson() {
		return new JSONObject(this.getOutputs());
	}
}
