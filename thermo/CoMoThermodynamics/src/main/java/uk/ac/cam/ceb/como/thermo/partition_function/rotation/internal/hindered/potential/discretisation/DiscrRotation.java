/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import uk.ac.cam.ceb.como.math.data.DataSpace;

/**
 *
 * @author pb556
 */
public class DiscrRotation {
    
    protected String id;
    protected DataSpace space;    
    protected RotationalMode mode;
    
    public DiscrRotation(String id, DataSpace space, RotationalMode mode) {
        this.id = id;
        this.mode = mode;
        this.space = space;
    }
    
    public String getId() {
        return id;
    }
    
    public DataSpace getDataSpace() {
        return space;
    }
    
    public RotationalMode getRotationalMode() {
        return mode;
    }
}
