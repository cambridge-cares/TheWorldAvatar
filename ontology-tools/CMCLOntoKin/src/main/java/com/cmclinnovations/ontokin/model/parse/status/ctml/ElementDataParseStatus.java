package com.cmclinnovations.ontokin.model.parse.status.ctml;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not a CTML element metadata element or attribute has 
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class ElementDataParseStatus {
	boolean elementData = false;
	boolean elementDataId = false;
	boolean elementDataCaseSensitive = false;
	
	public boolean isElementData() {
		return elementData;
	}
	public void setElementData(boolean elementData) {
		this.elementData = elementData;
	}
	public boolean isElementDataId() {
		return elementDataId;
	}
	public void setElementDataId(boolean elementDataId) {
		this.elementDataId = elementDataId;
	}
	public boolean isElementDataCaseSensitive() {
		return elementDataCaseSensitive;
	}
	public void setElementDataCaseSensitive(boolean elementDataCaseSensitive) {
		this.elementDataCaseSensitive = elementDataCaseSensitive;
	}
}
