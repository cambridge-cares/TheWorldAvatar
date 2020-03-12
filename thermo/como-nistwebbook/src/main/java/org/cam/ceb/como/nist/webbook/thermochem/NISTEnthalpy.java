/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.webbook.thermochem;

/**
 * A model defined to represent the enthapy of the current species as</br> 
 * numerical values.
 * 
 * @author pb556
 */
public class NISTEnthalpy {

    protected double value = 0.0;
    protected double posTolerance = 0.0;
    protected double negTolerance = 0.0;
    protected String units = "";
    protected String method = "";
    protected String reference = "";
    protected String comment = "";
    protected String reaction = "";
    
    protected EnthalpyType eType = EnthalpyType.NONE;
    protected ValueType vType = ValueType.NONE;
    
    public enum EnthalpyType {
        FORMATION, COMBUSTION, REACTION, NONE
    }
    
    public enum ValueType {
        UPPER_THRESHOLD, LOWER_THRESHOLD, EXACT, NONE
    }

    public void setValue(double value, ValueType valueType, EnthalpyType enthalpyType) {
        eType = enthalpyType;
        vType = valueType;
        this.value = value;
    }

    public void setTolerance(double pos, double neg) {
    	posTolerance = pos;
    	negTolerance = neg;
    }

    public void setUnits(String units) {
        this.units = units;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public void setReference(String reference) {
        this.reference = reference;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public double getValue() {
        return value;
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

    public String getMethod() {
        return method;
    }

    public String getReference() {
        return reference;
    }

    public String getComment() {
        return comment;
    }

    public EnthalpyType getEnthalpyType() {
        return eType;
    }
    
    public ValueType getValueType() {
        return vType;
    }

	public String getReaction() {
		return reaction;
	}

	public void setReaction(String reaction) {
		this.reaction = reaction;
	}
}
