package uk.ac.ceb.como.molhub.bean;

/**
 * 
 * The Class RotationalConstant.
 * <p>Class provides a bean for rotational constatnt data : rotational constant size, rotational constant value, rotational constant unit.</p>
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class RotationalConstant {

	/** The rotational constants size. */
	private String rotationalConstantsSize;	
	
	/** The rotational constants value. */
	private String rotationalConstantsValue;
	
	/** The rotational constants unit. */
	private String rotationalConstantsUnit;
	
	/**
	 * Instantiates a new rotational constant.
	 *
	 * @param rotationalConstantsSize the rotational constants size
	 * @param rotationalConstantsValue the rotational constants value
	 * @param rotationalConstantsUnit the rotational constants unit
	 */
	public RotationalConstant(String rotationalConstantsSize,String rotationalConstantsValue,String rotationalConstantsUnit) {
		
		this.rotationalConstantsSize=rotationalConstantsSize;
		this.rotationalConstantsValue=rotationalConstantsValue;
		this.rotationalConstantsUnit=rotationalConstantsUnit;
	}
	
	/**
	 * Gets the rotational constants size.
	 *
	 * @return the rotational constants size
	 */
	public String getRotationalConstantsSize() {
		return rotationalConstantsSize;
	}
	
	/**
	 * Sets the rotational constants size.
	 *
	 * @param rotationalConstantsSize the new rotational constants size
	 */
	public void setRotationalConstantsSize(String rotationalConstantsSize) {
		this.rotationalConstantsSize = rotationalConstantsSize;
	}
	
	/**
	 * Gets the rotational constants unit.
	 *
	 * @return the rotational constants unit
	 */
	public String getRotationalConstantsUnit() {
		return rotationalConstantsUnit;
	}
	
	/**
	 * Sets the rotational constants unit.
	 *
	 * @param rotationalConstantsUnit the new rotational constants unit
	 */
	public void setRotationalConstantsUnit(String rotationalConstantsUnit) {
		this.rotationalConstantsUnit = rotationalConstantsUnit;
	}
	
	/**
	 * Gets the rotational constants value.
	 *
	 * @return the rotational constants value
	 */
	public String getRotationalConstantsValue() {
		return rotationalConstantsValue;
	}
	
	/**
	 * Sets the rotational constants value.
	 *
	 * @param rotationalConstantsValue the new rotational constants value
	 */
	public void setRotationalConstantsValue(String rotationalConstantsValue) {
		this.rotationalConstantsValue = rotationalConstantsValue;
	}
	
	
}
