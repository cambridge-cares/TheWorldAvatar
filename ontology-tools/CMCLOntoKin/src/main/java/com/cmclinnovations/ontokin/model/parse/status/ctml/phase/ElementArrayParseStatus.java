package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not the element array tag of a phase and its attributes have
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class ElementArrayParseStatus {
	boolean elementArray = false;
	boolean elementDataSrc = false;
	
	public boolean isElementArray() {
		return elementArray;
	}
	public void setElementArray(boolean elementArray) {
		this.elementArray = elementArray;
	}
	public boolean isElementDataSrc() {
		return elementDataSrc;
	}
	public void setElementDataSrc(boolean elementDataSrc) {
		this.elementDataSrc = elementDataSrc;
	}
}
