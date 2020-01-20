/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import java.util.ArrayList;

/**
 *
 * @author pb556
 */
public class RotationalModes extends ArrayList<RotationalMode> {
    
    protected String freqUnit;
    protected String redMomentUnit;
    
//    public RotationalModes(String unit) {
//        
//    }
    
    public void setUnits(String freqUnit, String redMomentUnit) {
        this.freqUnit = freqUnit;
        this.redMomentUnit = redMomentUnit;
    }
    
    public void setFrequencyUnit(String unit) {
        freqUnit = unit;
    }
    
    public void setReducedMomentUnit(String unit) {
        redMomentUnit = unit;
    }
    
    public String getFrequencyUnit() {
        return freqUnit;
    }
    
    public String getReducedMomentUnit() {
        return redMomentUnit;
    }
}
