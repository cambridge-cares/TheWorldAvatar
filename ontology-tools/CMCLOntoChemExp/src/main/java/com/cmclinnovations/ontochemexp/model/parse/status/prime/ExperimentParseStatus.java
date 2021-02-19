package com.cmclinnovations.ontochemexp.model.parse.status.prime;
/**
 * This class contains getters and setters to flags that maintain whether
 * or not a PrIMe experiment's elements and attributes have already been parsed.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ExperimentParseStatus {
	boolean experiment = false;
	boolean primeID = false;
	boolean xmlns = false;
	boolean xmlnsXsi = false;
	boolean xsiSchemaLocation = false;

	public boolean isExperiment() {
		return experiment;
	}

	public void setExperiment(boolean experiment) {
		this.experiment = experiment;
	}
	
	public boolean isPrimeID() {
		return primeID;
	}
	
	public void setPrimeID(boolean primeID) {
		this.primeID = primeID;
	}
	
	public boolean isXmlns() {
		return xmlns;
	}
	
	public void setXmlns(boolean xmlns) {
		this.xmlns = xmlns;
	}
	
	public boolean isXmlnsXsi() {
		return xmlnsXsi;
	}
	
	public void setXmlnsXsi(boolean xmlnsXsi) {
		this.xmlnsXsi = xmlnsXsi;
	}
	
	public boolean isXsiSchemaLocation() {
		return xsiSchemaLocation;
	}
	
	public void setXsiSchemaLocation(boolean xsiSchemaLocation) {
		this.xsiSchemaLocation = xsiSchemaLocation;
	}
}
