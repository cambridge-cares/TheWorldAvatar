package com.cmclinnovations.jps.kg.object.model;

/**
 * Defined species with information that will be functional for generating</br> 
 * geometry using Open Babel. 
 * 
 * @author msff2
 *
 */
public class Species {
    private String smile; 
    private String inchi;
	public String getSmile() {
		return smile;
	}
	public void setSmile(String smile) {
		this.smile = smile;
	}
	public String getInchi() {
		return inchi;
	}
	public void setInchi(String inchi) {
		this.inchi = inchi;
	}
}
