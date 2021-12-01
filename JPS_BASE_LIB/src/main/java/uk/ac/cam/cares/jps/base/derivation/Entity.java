package uk.ac.cam.cares.jps.base.derivation;

public class Entity {
    private String iri;
    private String rdfType;
    private Derivation belongsTo; // belongsTo this derivation
    private boolean hasBelongsTo;
    private Derivation inputOf; // input of this derivation
    private boolean isInputToDerivation;
    private Long timestamp; // only if this is a pure input
    
    public Entity(String iri) {
    	this.iri = iri;
    	this.hasBelongsTo = false;
    	this.isInputToDerivation = false;
    }
    
    public String getIri() {
    	return this.iri;
    }
    
	public void setBelongsTo(Derivation derivation) {
    	this.belongsTo = derivation;
    	this.hasBelongsTo = true;
    }
	
	public void setAsInput(Derivation derivation) {
		this.inputOf = derivation;
		this.isInputToDerivation = true;
	}
	
	public Derivation getInputOf() {
		return this.inputOf;
	}
	
	public boolean isInputToDerivation() {
		return this.isInputToDerivation;
	}
	
	public void setTimestamp(Long timestamp) {
		this.timestamp = timestamp;
	}
	
	public Long getTimestamp() {
		return this.timestamp;
	}
	
	public boolean hasBelongsTo() {
		return this.hasBelongsTo;
	}
	
	public Derivation getBelongsTo() {
		return this.belongsTo;
	}
	
	public void setRdfType(String rdfType) {
		this.rdfType = rdfType;
	}
	
	public String getRdfType() {
		return this.rdfType;
	}
}
