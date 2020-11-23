package com.cmclinnovations.ontokin.model.parse.status.ctml;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not a Validate metadata element or attribute has already been parsed.
 * 
 * @author msff2
 *
 */
public class ValidateParseStatus {
	boolean validate = false;
	boolean reactionsToBeValidated = false;
	boolean speciesToBeValidated = false;
	
	public boolean isValidate() {
		return validate;
	}
	public void setValidate(boolean validate) {
		this.validate = validate;
	}
	public boolean isReactionsToBeValidated() {
		return reactionsToBeValidated;
	}
	public void setReactionsToBeValidated(boolean reactionsToBeValidated) {
		this.reactionsToBeValidated = reactionsToBeValidated;
	}
	public boolean isSpeciesToBeValidated() {
		return speciesToBeValidated;
	}
	public void setSpeciesToBeValidated(boolean speciesToBeValidated) {
		this.speciesToBeValidated = speciesToBeValidated;
	}
}
