package uk.ac.ceb.como.molhub.bean;

/**
 * 
 * @author nk510
 * Class provides a bean for atomic mass data : atomic mass value, atomic mass unit, and atom name.
 */
public class AtomicMass {
	
	
	private String atomicMassValue;
	private String atomicMassUnit;
	private String atomName;
	
	public AtomicMass(String atomName, String atomicMassValue, String atomicMassUnit) {
		
		this.atomName=atomName;
		this.atomicMassValue=atomicMassValue;
		this.atomicMassUnit=atomicMassUnit;
	}
	
	public String getAtomicMassValue() {
		return atomicMassValue;
	}
	public void setAtomicMassValue(String atomicMassValue) {
		this.atomicMassValue = atomicMassValue;
	}
	public String getAtomicMassUnit() {
		return atomicMassUnit;
	}
	public void setAtomicMassUnit(String atomicMassUnit) {
		this.atomicMassUnit = atomicMassUnit;
	}
	public String getAtomName() {
		return atomName;
	}
	public void setAtomName(String atomName) {
		this.atomName = atomName;
	}
	
	
}
