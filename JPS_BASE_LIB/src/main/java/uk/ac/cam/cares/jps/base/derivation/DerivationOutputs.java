package uk.ac.cam.cares.jps.base.derivation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DerivationOutputs {
	public static final String RETRIEVED_INPUTS_TIMESTAMP_KEY = "retrievedInputsAt";
	private Map<String, List<String>> outputs;
	private long retrievedInputsAt;

	public DerivationOutputs(JSONObject mappedInstances) {
		Map<String, List<String>> map = new HashMap<>();
		Iterator<String> keys = mappedInstances.keys();
		while (keys.hasNext()) {
			String key = keys.next();
			Object val = mappedInstances.get(key);
			if (val instanceof JSONArray) {
				map.put(key, ((JSONArray) val).toList().stream().map(iri -> (String) iri).collect(Collectors.toList()));
			} else if (val instanceof String) {
				map.put(key, new ArrayList<>(Arrays.asList((String) val)));
			} else {
				throw new JPSRuntimeException("Serilise the given JSONObject to DerivationOutputs is not supported:"
						+ mappedInstances.toString());
			}
		}
		this.outputs = map;
	}

	public DerivationOutputs(String rdfType, List<String> iris) {
		this.outputs = new HashMap<>();
		this.outputs.put(rdfType, new ArrayList<>(iris));
	}

	public DerivationOutputs(String rdfType, String iri) {
		this.outputs = new HashMap<>();
		this.outputs.put(rdfType, new ArrayList<>(Arrays.asList(iri)));
	}

	public DerivationOutputs(Map<String, List<String>> outputs) {
		this.outputs = new HashMap<>();
		outputs.forEach((key, lst) -> this.outputs.put(key, new ArrayList<>(lst)));
	}

	public void setRetrievedInputsAt(long timestamp) {
		this.retrievedInputsAt = timestamp;
	}

	public long getRetrievedInputsAt() {
		return this.retrievedInputsAt;
	}

	public void addToOutputs(String rdfType, List<String> iris) {
		if (!this.outputs.containsKey(rdfType)) {
			this.outputs.put(rdfType, new ArrayList<>(iris));
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

	public List<String> getNewDerivedIRI() {
		return this.getOutputs().values().stream().flatMap(i -> i.stream()).collect(Collectors.toList());
	}

	public JSONObject toJson() {
		return new JSONObject(this.getOutputs());
	}
}
