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

public class DerivationInputs {
	private Map<String, List<String>> inputs;
	private String derivationIRI;

	public static final String DERIVATIONINPUTS_SERIALISE_ERROR = "Serialise the given JSONObject to DerivationInputs is not supported: ";

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
	public DerivationInputs(JSONObject mappedInputs, String derivationIRI) {
		Map<String, List<String>> map = new HashMap<>();
		Iterator<String> keys = mappedInputs.keys();
		while (keys.hasNext()) {
			String key = keys.next();
			Object val = mappedInputs.get(key);
			if (val instanceof JSONArray) {
				map.put(key, ((JSONArray) val).toList().stream().map(iri -> trimIRI((String) iri))
						.collect(Collectors.toList()));
			} else if (val instanceof String) {
				map.put(key, new ArrayList<>(Arrays.asList(trimIRI((String) val))));
			} else {
				throw new JPSRuntimeException(DERIVATIONINPUTS_SERIALISE_ERROR + mappedInputs.toString());
			}
		}
		this.inputs = map;
		this.derivationIRI = derivationIRI;
	}

	/**
	 * The constructor to serilise a list of Entity to DerivationInputs.
	 * 
	 * @param entities
	 */
	public DerivationInputs(List<Entity> entities, String derivationIRI) {
		Map<String, List<String>> map = new HashMap<>();
		for (Entity entity : entities) {
			if (map.containsKey(entity.getRdfType())) {
				map.get(entity.getRdfType()).add(entity.getIri());
			} else {
				map.put(entity.getRdfType(), new ArrayList<>(Arrays.asList(entity.getIri())));
			}
		}
		this.inputs = map;
		this.derivationIRI = derivationIRI;
	}

	/**
	 * The constructor to serilise a Map<String, List<String>> to DerivationInputs.
	 * This constructor is provided to communicate with the pyderivationagent when
	 * handling derivations.
	 * 
	 * @param inputs
	 */
	public DerivationInputs(Map<String, List<String>> inputs, String derivationIRI) {
		this.inputs = inputs;
		this.derivationIRI = derivationIRI;
	}

	public String getDerivationIRI() {
		return this.derivationIRI;
	}

	public Map<String, List<String>> getInputs() {
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

	public boolean containsRdfType(String rdfType) {
		return this.getInputs().containsKey(rdfType);
	}

	public List<String> getAllIris() {
		return this.getInputs().values().stream().flatMap(i -> i.stream()).collect(Collectors.toList());
	}

	public void addToInputs(String rdfType, List<String> iris) {
		rdfType = trimIRI(rdfType);
		iris = trimIRI(iris);
		if (!this.inputs.containsKey(rdfType)) {
			this.inputs.put(rdfType, new ArrayList<>(iris));
		} else {
			this.inputs.get(rdfType).addAll(iris);
		}
	}

	public void addToInputs(String rdfType, String iri) {
		this.addToInputs(rdfType, Arrays.asList(iri));
	}

	@Override
	public String toString() {
		return this.getInputs().toString();
	}

	String trimIRI(String iri) {
		if (iri.startsWith("<")) {
			iri = iri.substring(1);
		}
		if (iri.endsWith(">")) {
			iri = iri.substring(0, iri.length() - 1);
		}
		return iri;
	}

	List<String> trimIRI(List<String> iris) {
		return iris.stream().map(iri -> trimIRI(iri)).collect(Collectors.toList());
	}
}
