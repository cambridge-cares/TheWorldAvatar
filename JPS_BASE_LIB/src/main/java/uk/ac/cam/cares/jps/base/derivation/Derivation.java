package uk.ac.cam.cares.jps.base.derivation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.json.JSONObject;

public class Derivation {
	private String iri;
	private List<Entity> inputs; // isDerivedFrom
	// isDerivedFrom upstream derivations - when new info generation
	private List<Derivation> directedUpstreams;
	private boolean hasDirectedUpstreams = false;
	// downstream derivations isDerivedFrom - when new info generation
	private List<Derivation> directedDownstreams;
	private boolean hasDirectedDownstreams = false;
	private Long timestamp;
	private String agentURL; // isDerivedUsing
	private List<Entity> entities; // entities belongsTo this derivation
	private String rdfType;
	private boolean updated = false;

	public Derivation(String iri, String rdfType) {
		this.iri = iri;
		this.rdfType = rdfType;
		entities = new ArrayList<Entity>();
		inputs = new ArrayList<Entity>();
		directedUpstreams = new ArrayList<Derivation>();
		directedDownstreams = new ArrayList<Derivation>();
	}

	public String getIri() {
		return this.iri;
	}

	public void setTimestamp(Long timestamp) {
		this.timestamp = timestamp;
	}

	public void setAgentURL(String agentURL) {
		this.agentURL = agentURL;
	}

	public void addEntity(Entity entity) {
		if (!entities.stream().anyMatch(e -> e.getIri().equals(entity.getIri()))) {
			this.entities.add(entity);
			entity.setBelongsTo(this);
		}
	}

	public void replaceEntities(List<Entity> entities) {
		this.entities = new ArrayList<>();
		for (Entity entity : entities) {
			this.entities.add(entity);
			entity.setBelongsTo(this);
		}
	}

	public List<Entity> getEntities() {
		return this.entities;
	}

	public List<String> getEntitiesIri() {
		return this.entities.stream().map(e -> e.getIri()).collect(Collectors.toList());
	}

	public void addInput(Entity input) {
		if (!inputs.stream().anyMatch(i -> i.getIri().equals(input.getIri()))) {
			this.inputs.add(input);
			input.setAsInput(this);
		}
	}

	public void removeInput(Entity input) {
		this.inputs.remove(input);
	}

	public List<Entity> getInputs() {
		return this.inputs;
	}

	public Long getTimestamp() {
		return this.timestamp;
	}

	public List<Derivation> getInputsWithBelongsTo() {
		List<Entity> inputs = this.getInputs();

		List<Derivation> inputsWithBelongsTo = new ArrayList<>();
		for (Entity input : inputs) {
			if (input.hasBelongsTo()) {
				inputsWithBelongsTo.add(input.getBelongsTo());
			}
		}

		return inputsWithBelongsTo;
	}

	/**
	 * inputs for agent call
	 * 
	 * @param derivationIRI
	 * @return
	 */
	public List<String> getAgentInputs() {
		return this.getInputs().stream().map(i -> i.getIri()).collect(Collectors.toList());
	}

	public JSONObject getAgentInputsMap() {
		Map<String, List<String>> inputsMap = new HashMap<>();
		this.getInputs().stream().forEach(i -> {
			if (!inputsMap.containsKey(i.getRdfType())) {
				inputsMap.put(i.getRdfType(), new ArrayList<>(Arrays.asList(i.getIri())));
			} else {
				inputsMap.get(i.getRdfType()).add(i.getIri());
			}
		});
		return new JSONObject(inputsMap);
	}

	public boolean isOutOfDate() {
		boolean outOfDate = false;
		List<Entity> inputs = this.getInputs();

		for (Entity input : inputs) {
			long input_timestamp;
			if (input.hasBelongsTo()) {
				input_timestamp = input.getBelongsTo().getTimestamp();
			} else {
				input_timestamp = input.getTimestamp();
			}

			if (input_timestamp > this.timestamp) {
				outOfDate = true;
				return outOfDate;
			}
		}

		return outOfDate;
	}

	public String getAgentURL() {
		return this.agentURL;
	}

	public boolean isDerivationWithTimeSeries() {
		if (rdfType.equals(DerivationSparql.ONTODERIVATION_DERIVATIONWITHTIMESERIES)) {
			return true;
		} else {
			return false;
		}
	}

	public boolean isDerivationAsyn() {
		if (rdfType.equals(DerivationSparql.ONTODERIVATION_DERIVATIONASYN)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * this is used to update all the timestamps in the kg after all the derivations
	 * are updated
	 * 
	 * @param status
	 */
	public void setUpdateStatus(boolean status) {
		this.updated = status;
	}

	public boolean getUpdateStatus() {
		return this.updated;
	}

	/**
	 * @return List<Derivation> return the directedUpstreams
	 */
	public List<Derivation> getDirectedUpstreams() {
		return directedUpstreams;
	}

	public void setDirectedUpstreams(Derivation directedUpstream) {
		if (this.hasDirectedUpstreams) {
			if (this.directedUpstreams.stream().allMatch(d -> d.getIri() != directedUpstream.getIri())) {
				this.directedUpstreams.add(directedUpstream);
			}
		} else {
			this.directedUpstreams.add(directedUpstream);
			this.hasDirectedUpstreams = true;
		}
		// also make the reverse connection
		directedUpstream.setDirectedDownstreams(this);
	}

	/**
	 * @param directedUpstreams the directedUpstreams to set
	 */
	public void setDirectedUpstreams(List<Derivation> directedUpstreams) {
		directedUpstreams.forEach(d -> {
			this.setDirectedUpstreams(d);
		});
	}

	/**
	 * @return List<Derivation> return the directedDownstreams
	 */
	public List<Derivation> getDirectedDownstreams() {
		return directedDownstreams;
	}

	/**
	 * This method should NOT be called directly by developers.
	 * 
	 * @param directedDownstream
	 */
	public void setDirectedDownstreams(Derivation directedDownstream) {
		if (this.hasDirectedDownstreams) {
			if (this.directedDownstreams.stream().allMatch(d -> d.getIri() != directedDownstream.getIri())) {
				this.directedDownstreams.add(directedDownstream);
			}
		} else {
			this.directedDownstreams.add(directedDownstream);
			this.hasDirectedDownstreams = true;
		}
	}

	/**
	 * This method should NOT be called directly by developers.
	 * 
	 * @param directedDownstreams the directedDownstreams to set
	 */
	public void setDirectedDownstreams(List<Derivation> directedDownstreams) {
		directedDownstreams.forEach(d -> {
			this.setDirectedDownstreams(d);
		});
	}
}
