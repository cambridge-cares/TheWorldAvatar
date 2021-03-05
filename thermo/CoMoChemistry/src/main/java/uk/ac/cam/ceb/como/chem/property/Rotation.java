/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.chem.property;

import uk.ac.cam.ceb.como.chem.structure.Bond;

/**
 *
 * @author pb556
 */
public class Rotation extends Vibration {
    
    protected int symm = 0;
    protected int periodicity = 0;
    protected int multiplicity = 0;
    protected double redMoment = 0.0;
    protected double barrierHeight = 0.0;
    
    protected double rotFreq = 0.0;
    
    protected Bond bond = null;
    
    public double getReducedMoment() {
        return redMoment;
    }
    
    public double getBarrierHeight() {
        return barrierHeight;
    }
    
    public void setBarrierHeight(double v) {
        barrierHeight = v;
    }
    
    public double getRotationalFrequency() {
        return rotFreq;
    }
    
    public int getSymmetryNumber() {
        return symm;
    }
    
    public int getPeriodicity() {
        return periodicity;
    }
    
    public int getMultiplicity() {
        return multiplicity;
    }
    
    public Bond getTorsionalBond() {
        return bond;
    }
    
    public void setTorsionalBond(Bond bond) {
        this.bond = bond;
    }
    
    public void setReducedMoment(double redMoment) {
        this.redMoment = redMoment;
    }
    
//    public double getReducedMoment() {
//        return redMoment;
//    }
//    
//    public VibrationalNormalMode getVibrationalNormalMode() {
//        return vibMode;
//    }
//    
//    public Top getTop() {
//        return top;
//    }
    
    public void setRotationalFrequency(double freq) {
        this.rotFreq = freq;
    }
    
    public void setSymmetryNumber(int s) {
        symm = s;
    }
    
    public void setPeriodicity(int periodicity) {
        this.periodicity = periodicity;
    }
    
    public void setMultiplicity(int multiplicity) {
        this.multiplicity = multiplicity;
    }
    
    @Override
    public Object clone() throws CloneNotSupportedException {
        Rotation clone = new Rotation();
        clone.setForceConstant(getForceConstant());
        clone.setFrequency(getFrequency());
        clone.setIRInten(getIRInten());
        clone.setMode(getMode());
        clone.setReducedMass(getReducedMass());
        clone.setTorsionalBond(clone.getTorsionalBond());
        clone.setMultiplicity(clone.getMultiplicity());
        clone.setPeriodicity(clone.getPeriodicity());
        clone.setRotationalFrequency(clone.getRotationalFrequency());
        clone.setSymmetryNumber(clone.getSymmetryNumber());
        clone.setDisplacements((DisplacementList) getDisplacements().clone());
        return clone;
    }
    
//    public void setReducedMoment(double redMoment) {
//        this.redMoment = redMoment;
//    }
//    
//    public void setVibrationalNormalMode(VibrationalNormalMode vibMode) {
//        this.vibMode = vibMode;
//    }
    
//    public void setTop(Top top) {
//        this.top = top;
//    }
}
