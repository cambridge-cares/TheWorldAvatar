package org.cam.ceb.como.nist.webbook.thermochem;

import org.cam.ceb.como.nist.webbook.info.NISTTemperature;
/**
 * A model defined to encode the enthalpy of formation, vaporisation,</br> 
 * sublimation or fusion.
 * 
 * @author msff2
 *
 */
public class NISTEnthalpyOfX {
	private String value;
	private String units;
	private NISTTemperature temperature;
	private String method;
	private String reference;
	private String comment;
	
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}
	public String getUnits() {
		return units;
	}
	public void setUnits(String units) {
		this.units = units;
	}
	public NISTTemperature getTemperature() {
		return temperature;
	}
	public void setTemperature(NISTTemperature temperature) {
		this.temperature = temperature;
	}
	public String getMethod() {
		return method;
	}
	public void setMethod(String method) {
		this.method = method;
	}
	public String getReference() {
		return reference;
	}
	public void setReference(String reference) {
		this.reference = reference;
	}
	public String getComment() {
		return comment;
	}
	public void setComment(String comment) {
		this.comment = comment;
	}
}
