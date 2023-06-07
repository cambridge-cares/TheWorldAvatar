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

	private Status status; // for asynchronous derivation
	private String errMsg;

	/**
	 * Inner class Status, for asynchronous derivation (DerivationAsyn)
	 */
	class Status {
		private String statusIri;
		private String statusRdfType;
		private List<Entity> newDerivedIRI;

		public Status(String statusIri, String statusRdfType) {
			this.statusIri = statusIri;
			this.statusRdfType = statusRdfType;
			this.newDerivedIRI = new ArrayList<Entity>();
		}

		public String getStatusIri() {
			return this.statusIri;
		}

		public String getStatusRdfType() {
			return this.statusRdfType;
		}

		public void addNewDerivedIRI(Entity newEntity) {
			if (this.newDerivedIRI.stream().noneMatch(e -> e.getIri().equals(newEntity.getIri()))) {
				this.newDerivedIRI.add(newEntity);
			}
		}

		public List<Entity> getNewDerivedIRI() {
			return this.newDerivedIRI;
		}
	}

	public Derivation(String iri, String rdfType) {
		this.iri = iri;
		this.rdfType = rdfType;
		entities = new ArrayList<Entity>();
		inputs = new ArrayList<Entity>();
		directedUpstreams = new ArrayList<Derivation>();
		directedDownstreams = new ArrayList<Derivation>();
	}

	public void setErrMsg(String errMsg) {
		this.errMsg = errMsg;
	}

	public String getErrMsg() {
		return this.errMsg;
	}

	public String getIri() {
		return this.iri;
	}

	public String getRdfType() {
		return this.rdfType;
	}

	public void setTimestamp(Long timestamp) {
		this.timestamp = timestamp;
	}

	public void setAgentURL(String agentURL) {
		this.agentURL = agentURL;
	}

	public void addEntity(Entity entity) {
		if (entities.stream().noneMatch(e -> e.getIri().equals(entity.getIri()))) {
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
		return this.entities.stream().map(Entity::getIri).collect(Collectors.toList());
	}

	public Status getStatus() {
		return this.status;
	}

	public void setStatus(String statusIri, String statusRdfType) {
		this.status = new Status(statusIri, statusRdfType);
	}

	public void addInput(Entity input) {
		if (inputs.stream().noneMatch(i -> i.getIri().equals(input.getIri()))) {
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

		List<Derivation> inputsWithBelongsTo = new ArrayList<>();
		for (Entity input : this.getInputs()) {
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
		return this.getInputs().stream().map(Entity::getIri).collect(Collectors.toList());
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

	public JSONObject getBelongsToMap() {
		Map<String, String> belongsToMap = new HashMap<>();
		this.getEntities().stream().forEach(e ->
			belongsToMap.put(e.getIri(), e.getRdfType()));
		return new JSONObject(belongsToMap);
	}

	public List<String> getBelongsToIris(String rdfType) {
		return this.getEntities().stream().filter(e -> e.getRdfType().equals(rdfType)).map(Entity::getIri)
				.collect(Collectors.toList());
	}

	public JSONObject getDownstreamDerivationMap() {
		Map<String, List<String>> downstreamDerivationMap = new HashMap<>();
		this.getEntities().stream().filter(e -> e.isInputToDerivation()).forEach(e -> {
			downstreamDerivationMap.put(e.getIri(),
					e.getInputOf().stream().map(dd -> dd.getIri()).collect(Collectors.toList()));
		});
		return new JSONObject(downstreamDerivationMap);
	}

	public boolean isOutOfDate() {
		boolean outOfDate = false;

		for (Entity input : this.getInputs()) {
			long inputTimestamp;
			if (input.hasBelongsTo()) {
				inputTimestamp = input.getBelongsTo().getTimestamp();
			} else {
				inputTimestamp = input.getTimestamp();
			}

			if (inputTimestamp > this.timestamp) {
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
			if (this.directedUpstreams.stream().allMatch(d -> !Objects.equals(d.getIri(), directedUpstream.getIri()))) {
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
		directedUpstreams.forEach(this::setDirectedUpstreams);
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
			if (this.directedDownstreams.stream().allMatch(d -> !Objects.equals(d.getIri(), directedDownstream.getIri()))) {
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
