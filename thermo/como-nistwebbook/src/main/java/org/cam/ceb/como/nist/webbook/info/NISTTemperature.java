package org.cam.ceb.como.nist.webbook.info;

import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy.ValueType;

/**
 * A model created to encode different temperatures of species.   
 * 
 * @author msff2
 *
 */
public class NISTTemperature {
	private double value;
    protected double posTolerance = 0.0;
    protected double negTolerance = 0.0;
	private String units;
	private String method;
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

	public double getValue() {
		return value;
	}
	public void setValue(double value) {
		this.value = value;
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
