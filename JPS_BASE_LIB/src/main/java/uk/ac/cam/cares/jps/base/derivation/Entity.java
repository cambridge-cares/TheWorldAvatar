package uk.ac.cam.cares.jps.base.derivation;

public class Entity {
    private String iri;
    private Derivation belongsTo; // belongsTo this derivation
    private boolean hasBelongsTo;
    private Derivation inputOf; // input of this derivation
    private long timestamp; // only if this is a pure input
    
    public Entity(String iri) {
    	this.iri = iri;
    	this.hasBelongsTo = false;
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
	}
	
	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}
	
	public long getTimestamp() {
		return this.timestamp;
	}
	
	public boolean hasBelongsTo() {
		return this.hasBelongsTo;
	}
	
	public Derivation getBelongsTo() {
		return this.belongsTo;
	}
}
