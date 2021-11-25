package uk.ac.cam.cares.jps.base.derivation;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Derivation{
	private String iri;
	private List<Entity> inputs; // isDerivedFrom
	private long timestamp;
	private String agentURL; // isDerivedUsing
	private List<Entity> entities; // entities belongsTo this derivation
	private String rdfType;
	
	public Derivation(String iri, String rdfType) {
		this.iri = iri;
		this.rdfType = rdfType;
		entities = new ArrayList<Entity>();
		inputs = new ArrayList<Entity>();
	}
	
	public String getIri() {
		return this.iri;
	}
	
	public void setTimestamp(long timestamp) {
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
	
	public List<Entity> getEntities() {
		return this.entities;
	}
	
	public void addInput(Entity input) {
		if (!inputs.stream().anyMatch(i -> i.getIri().equals(input.getIri()))) {
			this.inputs.add(input);
			input.setAsInput(this);
		}
	}
	
	public List<Entity> getInputs() {
		return this.inputs;
	}
	
	public long getTimestamp() {
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
     * @param derivationIRI
     * @return
     */
    public List<String> getAgentInputs() {    	
    	return this.getInputs().stream().map(i -> i.getIri()).collect(Collectors.toList());
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
    	if (rdfType.equals(DerivationSparql.derivednamespace + "DerivationWithTimeSeries")) {
    		return true;
    	} else {
    		return false;
    	}
    }
}
