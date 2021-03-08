/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import uk.ac.cam.ceb.como.chem.property.Vibration;

/**
 *
 * @author pb556
 */
public class RotationalMode {
    
    protected int symm = 0;
    protected int periodicity = 0;
    protected int multiplicity = 0;
    
    protected double freq = 0.0;
    protected double redMoment = 0.0;
    
    protected Vibration vibMode = null;
    
    protected Top top = null;
    
    public double getFrequency() {
        return freq;
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
    
    public double getReducedMoment() {
        return redMoment;
    }
    
    public Vibration getVibrationalNormalMode() {
        return vibMode;
    }
    
    public Top getTop() {
        return top;
    }
    
    public void setFrequency(double freq) {
        this.freq = freq;
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
    
    public void setReducedMoment(double redMoment) {
        this.redMoment = redMoment;
    }
    
    public void setVibrationalNormalMode(Vibration vibMode) {
        this.vibMode = vibMode;
    }
    
    public void setTop(Top top) {
        this.top = top;
    }
}
