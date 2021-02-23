package com.cmclinnovations.ontochem.model.reference.data.structure;
/**
 * Represents the model of a journal.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public class Journal {
	
	private String iSSN;
	
	private String title;

	public String getiSSN() {
		return iSSN;
	}

	public void setiSSN(String iSSN) {
		this.iSSN = iSSN;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}
}
