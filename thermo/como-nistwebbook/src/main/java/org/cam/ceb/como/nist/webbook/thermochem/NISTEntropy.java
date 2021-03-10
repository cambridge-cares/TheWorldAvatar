/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.thermochem;

import org.cam.ceb.como.nist.webbook.info.NISTTemperature;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy.ValueType;

/**
 * A model defined to represent the entropy of the current species as</br> 
 * numerical values as opposed to string values.
 * 
 * @author msff2
 */
public class NISTEntropy {
    protected double value = 0.0;
    protected double posTolerance = 0.0;
    protected double negTolerance = 0.0;
    protected String units = "";
    protected String method = "";
    protected String reference = "";
    protected String comment = "";
    protected NISTTemperature temperature;
    protected String reaction = "";
    
    protected ValueType vType = ValueType.NONE;
    
    public void setValue(double value, ValueType valueType) {
        vType = valueType;
        this.value = value;
    }

    public void setTolerance(double pos, double neg) {
    	posTolerance = pos;
    	negTolerance = neg;
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

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
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

	public ValueType getvType() {
		return vType;
	}

	public void setvType(ValueType vType) {
		this.vType = vType;
	}

	public NISTTemperature getTemperature() {
		return temperature;
	}

	public void setTemperature(NISTTemperature temperature) {
		this.temperature = temperature;
	}

	public String getReaction() {
		return reaction;
	}

	public void setReaction(String reaction) {
		this.reaction = reaction;
	}
}
