/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.approximation;

import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.DiscrRotation;

/**
 *
 * @author pb556
 */
public abstract class TorsionalBarrierApproximation<T> extends Approximation<T> {

    protected DiscrRotation rotation;
    
    public TorsionalBarrierApproximation(DiscrRotation rotation) {
        super();
        this.rotation = rotation;
    }
    
    public void setRotation(DiscrRotation rotation) {
        this.rotation = rotation;
    }
    
    public DiscrRotation getRotation() {
        return this.rotation;
    }    
}
