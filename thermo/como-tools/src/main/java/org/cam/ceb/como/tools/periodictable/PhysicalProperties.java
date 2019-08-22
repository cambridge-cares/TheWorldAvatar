/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

/**
 *
 * @author pb556
 */
public class PhysicalProperties {
    protected Phase phase;
    protected double stdAtomicWeight;
    protected double meltingPoint;
    protected double boilingPoint;
    protected double triplePoint;
    protected double criticalPoint;
    protected String colour;
    
    public void setColour(String colour) {
        this.colour = colour;
    }
    
    public void setPhase(Phase phase) {
        this.phase = phase;
    }
    
    public void setDensity() {
        
    }
}
