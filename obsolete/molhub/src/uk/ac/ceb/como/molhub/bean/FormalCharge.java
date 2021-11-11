package uk.ac.ceb.como.molhub.bean;

/**
 * The Class FormalCharge.
 * 
 * <p> Class provides a bean for formal charge data : formal charge value, formal charge unit. </p>
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 *  
 */

public class FormalCharge {

	/** The formal charge value. */
	String formalChargeValue;
	
	/** The formal charge unit. */
	String formalChargeUnit;
	
	/**
	 * Instantiates a new formal charge.
	 *
	 * @param formalChargeValue the formal charge value
	 * @param formalChargeUnit the formal charge unit
	 */
	public FormalCharge(String formalChargeValue, String formalChargeUnit) {
		
		this.formalChargeValue=formalChargeValue;
		this.formalChargeUnit=formalChargeUnit;
		
	}

	/**
	 * Gets the formal charge value.
	 *
	 * @return the formal charge value
	 */
	public String getFormalChargeValue() {
		return formalChargeValue;
	}

	/**
	 * Sets the formal charge value.
	 *
	 * @param formalChargeValue the new formal charge value
	 */
	public void setFormalChargeValue(String formalChargeValue) {
		this.formalChargeValue = formalChargeValue;
	}

	/**
	 * Gets the formal charge unit.
	 *
	 * @return the formal charge unit
	 */
	public String getFormalChargeUnit() {
		return formalChargeUnit;
	}

	/**
	 * Sets the formal charge unit.
	 *
	 * @param formalChargeUnit the new formal charge unit
	 */
	public void setFormalChargeUnit(String formalChargeUnit) {
		this.formalChargeUnit = formalChargeUnit;
	}
	
	
}
