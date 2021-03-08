/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.thermochem;

import org.cam.ceb.como.nist.webbook.info.NISTTemperature;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy.ValueType;

/**
 * A model created to encode different heat capacities of species.   
 *
 * @author pb556
 * @author msff2
 */
public class NISTHeatCapacity {
    protected double value = 0.0;
    protected double posTolerance = 0.0;
    protected double negTolerance = 0.0;
	private String units;
	private NISTTemperature temperature;
	private String reference;
	private String comment;
	
    protected ValueType vType = ValueType.NONE;
    
    public void setValue(double value, ValueType valueType) {
        vType = valueType;
        this.value = value;
    }

    public void setTolerance(double pos, double neg) {
    	posTolerance = pos;
    	negTolerance = neg;
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
	public double getValue() {
		return value;
	}
	public void setValue(double value) {
		this.value = value;
	}
	public double getPosTolerance() {
		return posTolerance;
	}
	public void setPosTolerance(double posTolerance) {
		this.posTolerance = posTolerance;
	}
	public double getNegTolerance() {
		return negTolerance;
	}
	public void setNegTolerance(double negTolerance) {
		this.negTolerance = negTolerance;
	}
}
