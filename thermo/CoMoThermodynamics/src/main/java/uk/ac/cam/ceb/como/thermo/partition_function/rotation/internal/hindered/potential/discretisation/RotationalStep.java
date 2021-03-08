/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation;

import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class RotationalStep {

    protected CMLMolecule mol;
    protected double angle;

    public RotationalStep(CMLMolecule mol, double angle) {
        this.mol = mol;
        this.angle = angle;
    }

    public double getAngle() {
        return angle;
    }

    public CMLMolecule getCMLMolecule() {
        return mol;
    }
}
