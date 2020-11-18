package com.cmclinnovations.ontokin.model.parse.status.ctml.elementdata;

/**
 * This class contains getters and setters to flags that maintain whether
 * or not a CTML element attribute or comment has already been parsed.
 * 
 * @author msff2
 *
 */
public class ElementParseStatus {
	boolean elementDataElement = false;
	boolean elementName = false;
	boolean elementAtomicWt = false;
	boolean elementAtomicWtUnits = false;
	boolean elementComment = false;

	public boolean isElementDataElement() {
		return elementDataElement;
	}
	public void setElementDataElement(boolean elementDataElement) {
		this.elementDataElement = elementDataElement;
	}
	public boolean isElementName() {
		return elementName;
	}
	public void setElementName(boolean elementName) {
		this.elementName = elementName;
	}
	public boolean isElementAtomicWt() {
		return elementAtomicWt;
	}
	public void setElementAtomicWt(boolean elementAtomicWt) {
		this.elementAtomicWt = elementAtomicWt;
	}
	public boolean isElementAtomicWtUnits() {
		return elementAtomicWtUnits;
	}
	public void setElementAtomicWtUnits(boolean elementAtomicWtUnits) {
		this.elementAtomicWtUnits = elementAtomicWtUnits;
	}
	public boolean isElementComment() {
		return elementComment;
	}
	public void setElementComment(boolean elementComment) {
		this.elementComment = elementComment;
	}
}
