/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.util;

import uk.ac.cam.ceb.como.chem.property.Vibration;

/**
 *
 * @author pb556
 */
public class SimpleVibNormalMode {
    
    protected String label;
    protected double freq;
    protected double redMass;
    protected double irInternal;
    protected double forceConst;
    
    public SimpleVibNormalMode(String label, double freq, double redMass, double irInternal, double forceConst) {
        this.label = label;
        this.freq = freq;
        this.redMass = redMass;
        this.irInternal = irInternal;
        this.forceConst = forceConst;                
    }
    
    public String getLabel() {
        return label;
    }
    
    public int getMode() {
        return Integer.parseInt(label.replaceFirst("f", ""));
    }
    
    public double getFrequency() {
        return freq;
    }
    
    public double getReducedMass() {
        return redMass;
    }
    
    public double getIRInternal() {
        return irInternal;
    }
    
    public double getForceConstant() {
        return forceConst;
    }
    
    public boolean equals(SimpleVibNormalMode mode) {
        double tolerance = 0.01;
        return Math.abs(freq - mode.freq) < tolerance &&
                 Math.abs(redMass - mode.redMass) < tolerance &&
                 Math.abs(irInternal - mode.irInternal) < tolerance &&
                 Math.abs(forceConst - mode.forceConst) < tolerance;
    }
    
    public boolean equals(Vibration mode) {
        double tolerance = 0.01;
        return Math.abs(forceConst - mode.getForceConstant()) < tolerance &&
                Math.abs(freq - mode.getFrequency()) < tolerance &&
                Math.abs(irInternal - mode.getIRInten()) < tolerance &&
                Math.abs(redMass - mode.getReducedMass()) < tolerance &&
                getMode() == mode.getMode();
    }
}
