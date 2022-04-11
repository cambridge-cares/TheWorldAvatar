package uk.ac.cam.cares.jps.base.derivation;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;

public class DerivationInputs {
	private Map<String, List<String>> inputs;

	/**
	 * The constructor to serilise JSONObject mappedInputs to DerivationInputs.
	 * The mappedInputs should have data structure similar to below:
	 * {"http://example.com/ClassA":
	 * "http://example.com/InstanceOfClassA",
	 * "http://example.com/ClassB":
	 * ["http://example.com/InstanceOneOfClassB","http://example.com/InstanceTwoOfClassB"],
	 * "http://example.com/ClassC": "http://example.com/InstanceOfClassC"}
	 * 
	 * @param mappedInputs
	 */
	public DerivationInputs(JSONObject mappedInputs) {
		Map<String, List<String>> map = new HashMap<>();
		Iterator<String> keys = mappedInputs.keys();
		while (keys.hasNext()) {
			String key = keys.next();
			Object val = mappedInputs.get(key);
			if (val instanceof JSONArray) {
				map.put(key, ((JSONArray) val).toList().stream().map(iri -> (String) iri).collect(Collectors.toList()));
			} else if (val instanceof String) {
				map.put(key, Arrays.asList((String) val));
			}
		}
		this.inputs = map;
	}

	/**
	 * The constructor to serilise a list of Entity to DerivationInputs.
	 * 
	 * @param entities
	 */
	public DerivationInputs(List<Entity> entities) {
		Map<String, List<String>> map = new HashMap<>();
		for (Entity entity : entities) {
			if (map.containsKey(entity.getRdfType())) {
				map.get(entity.getRdfType()).add(entity.getIri());
			} else {
				map.put(entity.getRdfType(), Arrays.asList(entity.getIri()));
			}
		}
		this.inputs = map;
	}

	private Map<String, List<String>> getInputs() {
		return this.inputs;
	}

	/**
	 * Method to retrieve the list of IRIs given the rdf:type.
	 * 
	 * @param rdfType
	 * @return
	 */
	public List<String> getIris(String rdfType) {
		// TODO do we need to throw exception when there's no key rdfType?
		return this.getInputs().get(rdfType);
	}

	@Override
	public String toString() {
		return this.getInputs().toString();
	}
}
