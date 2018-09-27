package uk.ac.ceb.como.molhub.bean;

public class Frequency {
	
	private String frequenciesSize;
	
	private String frequenciesValue;
	
	private String frequenciesUnit;
	
	public Frequency(){
		
	}
	public Frequency(String frequenciesSize, String frequenciesValue, String frequenciesUnit){
		
		this.frequenciesSize=frequenciesSize;
		this.frequenciesValue=frequenciesValue;
		this.frequenciesUnit=frequenciesUnit;
	}
	
	public String getFrequenciesValue() {
		return frequenciesValue;
	}
	public void setFrequenciesValue(String frequenciesValue) {
		this.frequenciesValue = frequenciesValue;
	}
	public String getFrequenciesUnit() {
		return frequenciesUnit;
	}
	public void setFrequenciesUnit(String frequenciesUnit) {
		this.frequenciesUnit = frequenciesUnit;
	}
	public String getFrequenciesSize() {
		return frequenciesSize;
	}
	public void setFrequenciesSize(String frequenciesSize) {
		this.frequenciesSize = frequenciesSize;
	}
	
}
