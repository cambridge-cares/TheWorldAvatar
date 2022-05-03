package uk.ac.cam.cares.jps.base.derivation;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class DerivationOutputs {
	public static final String RETRIEVED_INPUTS_TIMESTAMP_KEY = "retrievedInputsAt";
	private Map<String, String> oldEntitiesMap; // (iri, rdfType)
	private Map<String, List<String>> oldEntitiesDownstreamDerivationMap; // (iri, [derivation iri])
	private Map<String, List<String>> newEntitiesMap; // (rdfType, [iri])
	private List<TriplePattern> outputTriples;
	private String thisDerivation;
	private long retrievedInputsAt;

	public static final String PROTOCOLS = "^(ftp|file|https?)://.*$";
	public static final String OLD_ENTITIES_MAP_ERROR = "Serialise the given JSONObject to oldEntitiesMap Map<String, String> is not supported: ";
	public static final String OLD_ENTITIES_DOWNSTREAM_DERIVATION_MAP_ERROR = "Serialise the given JSONObject to oldEntitiesDownstreamDerivationMap Map<String, List<String>> is not supported: ";
	public static final String OLD_NEW_ENTITIES_MATCHING_ERROR = "When the agent writes new instances, make sure that there is 1 instance with matching rdf:type over the old set, old set: ";
	public static final String INVALID_IRI_ERROR = "Invalid IRI received when validating IRIs: ";

	//////////////////
	// Constructors //
	//////////////////

	public DerivationOutputs() {
		this.oldEntitiesMap = new HashMap<>();
		this.oldEntitiesDownstreamDerivationMap = new HashMap<>();
		this.newEntitiesMap = new HashMap<>();
		this.outputTriples = new ArrayList<TriplePattern>();
		this.thisDerivation = new String();
		this.retrievedInputsAt = 0;
	}

	////////////////////////
	// Set up information //
	////////////////////////

	public void setThisDerivation(String derivation) {
		this.thisDerivation = derivation;
	}

	public void setRetrievedInputsAt(long retrievedInputsAt) {
		this.retrievedInputsAt = retrievedInputsAt;
	}

	public void setOldEntitiesMap(JSONObject oldEntitiesRdfType) {
		// construct oldEntitiesMap
		Map<String, String> map = new HashMap<>();
		Iterator<String> keys = oldEntitiesRdfType.keys();
		while (keys.hasNext()) {
			String key = keys.next();
			Object val = oldEntitiesRdfType.get(key);
			if (val instanceof String) {
				map.put(key, (String) val);
			} else {
				throw new JPSRuntimeException(OLD_ENTITIES_MAP_ERROR + oldEntitiesRdfType.toString());
			}
		}
		this.oldEntitiesMap = map;
	}

	public void setOldEntitiesDownstreamDerivationMap(JSONObject oldEntitiesDownstreamDerivation) {
		// construct oldEntitiesDownstreamDerivationMap
		Map<String, List<String>> map = new HashMap<>();
		Iterator<String> keys = oldEntitiesDownstreamDerivation.keys();
		while (keys.hasNext()) {
			String key = keys.next();
			Object val = oldEntitiesDownstreamDerivation.get(key);
			if (val instanceof JSONArray) {
				map.put(key, ((JSONArray) val).toList().stream().map(iri -> (String) iri).collect(Collectors.toList()));
			} else if (val instanceof String) {
				map.put(key, new ArrayList<>(Arrays.asList((String) val)));
			} else {
				throw new JPSRuntimeException(
						OLD_ENTITIES_DOWNSTREAM_DERIVATION_MAP_ERROR + oldEntitiesDownstreamDerivation.toString());
			}
		}
		this.oldEntitiesDownstreamDerivationMap = map;
	}

	////////////////////////////////////////////////////////////////////
	// Create new entities and add triples - to be used by developers //
	////////////////////////////////////////////////////////////////////

	public void createNewEntity(String iri, String rdfType) {
		iri = trimIRI(iri);
		rdfType = trimIRI(rdfType);
		// add new entity to the entities mapping
		if (this.newEntitiesMap.containsKey(rdfType)) {
			if (!this.newEntitiesMap.get(rdfType).contains(iri)) {
				this.newEntitiesMap.get(rdfType).add(iri);
				// add new entity to triples
				this.addTriple(iri, RDF.TYPE.toString(), rdfType);
			}
		} else {
			this.newEntitiesMap.put(rdfType, new ArrayList<>(Arrays.asList(iri)));
			// add new entity to triples
			this.addTriple(iri, RDF.TYPE.toString(), rdfType);
		}
	}

	public void addTriple(TriplePattern triple) {
		this.addTriple(Arrays.asList(triple));
	}

	public void addTriple(List<TriplePattern> triples) {
		this.outputTriples.addAll(triples);
	}

	public void addTriple(String s, String p, String o) {
		s = trimIRI(s);
		p = trimIRI(p);
		o = trimIRI(o);
		checkIfValidIri(Arrays.asList(s, p));
		if (o.matches(PROTOCOLS)) {
			this.outputTriples.add(Rdf.iri(s).has(Rdf.iri(p), Rdf.iri(o)));
		} else {
			this.outputTriples.add(Rdf.iri(s).has(Rdf.iri(p), Rdf.literalOf(o)));
		}
	}

	public void addTriple(String s, String p, Number o) {
		s = trimIRI(s);
		p = trimIRI(p);
		checkIfValidIri(Arrays.asList(s, p));
		this.outputTriples.add(Rdf.iri(s).has(Rdf.iri(p), Rdf.literalOf(o)));
	}

	public void addTriple(String s, String p, Boolean o) {
		s = trimIRI(s);
		p = trimIRI(p);
		checkIfValidIri(Arrays.asList(s, p));
		this.outputTriples.add(Rdf.iri(s).has(Rdf.iri(p), Rdf.literalOf(o)));
	}

	/**
	 * Can be used to add triples with custom data type like:
	 * <http://9c9cf967-8ca8-4b44-a0c5-cad2098dd9eb>
	 * <http://09ee9702-8f34-4b81-8d57-5f294ebeafac>
	 * "48.13188#11.54965#1379714400"^^<http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon-time>
	 * .
	 * 
	 * @param s
	 * @param p
	 * @param o
	 * @param dataType
	 */
	public void addTriple(String s, String p, String o, String dataType) {
		s = trimIRI(s);
		p = trimIRI(p);
		checkIfValidIri(Arrays.asList(s, p, dataType));
		this.outputTriples.add(Rdf.iri(s).has(Rdf.iri(p), Rdf.literalOfType(o, Rdf.iri(dataType))));
	}

	//////////////////////////////////////////////////
	// Retrieve processed outputs for SPARQL update //
	//////////////////////////////////////////////////

	public List<TriplePattern> getOutputTriples() {
		return this.outputTriples;
	}

	public Map<String, List<String>> getNewEntitiesDownstreamDerivationMap() {
		Map<String, List<String>> newEntitiesDownstreamDerivationMap = new HashMap<>();
		this.newEntitiesMap.values().stream().flatMap(i -> i.stream())
				.forEach(iri -> newEntitiesDownstreamDerivationMap.put(iri, new ArrayList<>()));

		this.oldEntitiesDownstreamDerivationMap.forEach((iri, derivations) -> {
			List<String> matchingEntity = this.newEntitiesMap.get(this.oldEntitiesMap.get(iri));
			if (matchingEntity.size() != 1) {
				throw new JPSRuntimeException(OLD_NEW_ENTITIES_MATCHING_ERROR + this.oldEntitiesMap.toString()
						+ ", new entities" + this.newEntitiesMap.toString());
			}
			newEntitiesDownstreamDerivationMap.get(matchingEntity.get(0)).addAll(derivations);
		});
		return newEntitiesDownstreamDerivationMap;
	}

	public long getRetrievedInputsAt() {
		return this.retrievedInputsAt;
	}

	public String getThisDerivation() {
		return this.thisDerivation;
	}

	public List<String> getNewDerivedIRI() {
		return this.newEntitiesMap.values().stream().flatMap(i -> i.stream()).collect(Collectors.toList());
	}

	///////////////////////
	// Utility functions //
	///////////////////////

	void checkIfValidIri(List<String> strs) {
		strs.stream().forEach(s -> {
			try {
				new URI(trimIRI(s)).toURL();
			} catch (MalformedURLException | URISyntaxException | IllegalArgumentException e) {
				e.printStackTrace();
				throw new JPSRuntimeException(INVALID_IRI_ERROR + s);
			}
		});
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
}
