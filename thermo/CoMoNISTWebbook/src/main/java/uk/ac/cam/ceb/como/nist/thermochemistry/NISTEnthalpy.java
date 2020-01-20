/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.thermochemistry;

/**
 *
 * @author pb556
 */
public class NISTEnthalpy {

    protected double value = 0.0;
    protected double posTol = 0.0;
    protected double negTol = 0.0;
    protected String units = "";
    protected String method = "";
    protected String reference = "";
    protected String comment = "";
    
    protected EnthalpyType eType = EnthalpyType.NONE;
    protected ValueType vType = ValueType.NONE;
    
    public enum EnthalpyType {
        FORMATION, COMBUSTION, NONE
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
        posTol = pos;
        negTol = neg;
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
        return posTol;
    }

    public double getNegTolerance() {
        return negTol;
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
}
