package com.cmclinnovations.ontochemexp.model.parse.status.prime;

/**
 * This class contains getters and setters to flags that maintain whether
 * or not a PrIMe experiment's apparatus elements and attributes have already 
 * been parsed.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ApparatusParseStatus {
	boolean apparatus = false;

	public boolean isApparatus() {
		return apparatus;
	}
	public void setApparatus(boolean apparatus) {
		this.apparatus = apparatus;
	}
}
