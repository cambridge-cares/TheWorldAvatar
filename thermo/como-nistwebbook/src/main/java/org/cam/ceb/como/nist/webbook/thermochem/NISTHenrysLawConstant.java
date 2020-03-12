package org.cam.ceb.como.nist.webbook.thermochem;

import org.cam.ceb.como.nist.webbook.info.NISTTemperature;

/**
 * A model created to encode Henry's Law constants for calculating the
 * solubility of the current species.
 * 
 * @author msff2
 *
 */
public class NISTHenrysLawConstant {
	private double value;
	private String units;
	private double temperatureDependentConstant;
	private NISTTemperature temperature;
	private String method;
	private String reference;
	private String comment;
	private String solution;
	
	public double getValue() {
		return value;
	}

	public void setValue(double value) {
		this.value = value;
	}

	public double getTemperatureDependentConstant() {
		return temperatureDependentConstant;
	}

	public void setTemperatureDependentConstant(double temperatureDependentConstant) {
		this.temperatureDependentConstant = temperatureDependentConstant;
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

	public String getSolution() {
		return solution;
	}

	public void setSolution(String solution) {
		this.solution = solution;
	}

	public NISTTemperature getTemperature() {
		return temperature;
	}

	public void setTemperature(NISTTemperature temperature) {
		this.temperature = temperature;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}
}
