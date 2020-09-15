package uk.ac.ceb.como.molhub.bean;


/**
 * The Class AtomicMass.
 *
 * 
 * <p>Class provides a bean for atomic mass data : atomic mass value, atomic mass unit, and atom name.</p>
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 *  
 */
public class AtomicMass {
	
	
	/** The atomic mass value. */
	private String atomicMassValue;
	
	/** The atomic mass unit. */
	private String atomicMassUnit;
	
	/** The atom name. */
	private String atomName;
	
	/**
	 * Instantiates a new atomic mass.
	 *
	 * @param atomName the atom name
	 * @param atomicMassValue the atomic mass value
	 * @param atomicMassUnit the atomic mass unit
	 */
	public AtomicMass(String atomName, String atomicMassValue, String atomicMassUnit) {
		
		this.atomName=atomName;
		this.atomicMassValue=atomicMassValue;
		this.atomicMassUnit=atomicMassUnit;
	}
	
public AtomicMass(String atomName, String atomicMassValue) {
		
		this.atomName=atomName;
		this.atomicMassValue=atomicMassValue;
		
	}

	/**
	 * Gets the atomic mass value.
	 *
	 * @return the atomic mass value
	 */
	public String getAtomicMassValue() {
		return atomicMassValue;
	}
	
	/**
	 * Sets the atomic mass value.
	 *
	 * @param atomicMassValue the new atomic mass value
	 */
	public void setAtomicMassValue(String atomicMassValue) {
		this.atomicMassValue = atomicMassValue;
	}
	
	/**
	 * Gets the atomic mass unit.
	 *
	 * @return the atomic mass unit
	 */
	public String getAtomicMassUnit() {
		return atomicMassUnit;
	}
	
	/**
	 * Sets the atomic mass unit.
	 *
	 * @param atomicMassUnit the new atomic mass unit
	 */
	public void setAtomicMassUnit(String atomicMassUnit) {
		this.atomicMassUnit = atomicMassUnit;
	}
	
	/**
	 * Gets the atom name.
	 *
	 * @return the atom name
	 */
	public String getAtomName() {
		return atomName;
	}
	
	/**
	 * Sets the atom name.
	 *
	 * @param atomName the new atom name
	 */
	public void setAtomName(String atomName) {
		this.atomName = atomName;
	}
	
	
}
