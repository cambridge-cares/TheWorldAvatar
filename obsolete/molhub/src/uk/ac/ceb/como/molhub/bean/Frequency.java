package uk.ac.ceb.como.molhub.bean;

// TODO: Auto-generated Javadoc
/**
 * The Class Frequency.
 * 
 * 
 * <p>Class provides a bean for frequencies data : frequency value, frequency unit, and frequency size.</p>
 * 
 *  @author Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
 *  @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class Frequency {
	
	/** The frequencies size. */
	private String frequenciesSize;
	
	/** The frequencies value. */
	private String frequenciesValue;
	
	/** The frequencies unit. */
	private String frequenciesUnit;
	
	/**
	 * Instantiates a new frequency.
	 */
	public Frequency(){
		
	}
	
	/**
	 * Instantiates a new frequency.
	 *
	 * @param frequenciesSize the frequencies size
	 * @param frequenciesValue the frequencies value
	 * @param frequenciesUnit the frequencies unit
	 */
	public Frequency(String frequenciesSize, String frequenciesValue, String frequenciesUnit){
		
		this.frequenciesSize=frequenciesSize;
		this.frequenciesValue=frequenciesValue;
		this.frequenciesUnit=frequenciesUnit;
	}
	
	/**
	 * Gets the frequencies value.
	 *
	 * @return the frequencies value
	 */
	public String getFrequenciesValue() {
		return frequenciesValue;
	}
	
	/**
	 * Sets the frequencies value.
	 *
	 * @param frequenciesValue the new frequencies value
	 */
	public void setFrequenciesValue(String frequenciesValue) {
		this.frequenciesValue = frequenciesValue;
	}
	
	/**
	 * Gets the frequencies unit.
	 *
	 * @return the frequencies unit
	 */
	public String getFrequenciesUnit() {
		return frequenciesUnit;
	}
	
	/**
	 * Sets the frequencies unit.
	 *
	 * @param frequenciesUnit the new frequencies unit
	 */
	public void setFrequenciesUnit(String frequenciesUnit) {
		this.frequenciesUnit = frequenciesUnit;
	}
	
	/**
	 * Gets the frequencies size.
	 *
	 * @return the frequencies size
	 */
	public String getFrequenciesSize() {
		return frequenciesSize;
	}
	
	/**
	 * Sets the frequencies size.
	 *
	 * @param frequenciesSize the new frequencies size
	 */
	public void setFrequenciesSize(String frequenciesSize) {
		this.frequenciesSize = frequenciesSize;
	}
	
}
