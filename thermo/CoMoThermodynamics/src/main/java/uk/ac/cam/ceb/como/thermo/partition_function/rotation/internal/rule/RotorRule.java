/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.rule;

import java.util.Collection;
import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import java.util.List;

/**
 *
 * @author pb556
 */
public abstract class RotorRule {
    protected ThermoAnalyzable thermoAnalyzable = null;
    protected InternalRotorType irType = InternalRotorType.NONE;

    public enum InternalRotorType {
        NONE,
        FREE_ROTOR,
        CLASSICAL_LIMIT_HINDERED_ROTOR
    }

    public void setThermoAnalyzable(ThermoAnalyzable thermoAnalyzable) {
        this.thermoAnalyzable = thermoAnalyzable;
    }

    //public abstract double getEnergyBarrier(Bond rotorBond);

    public abstract double getReducedMOI(Bond rotorBond, Collection<Bond> allRotorBonds);
    
    public abstract double getReducedMOI(double angle, Bond rotorBond, Collection<Bond> allRotorBonds);
    
    public abstract int getRotorSymmetry(Bond rotorBond);

    public void setInternalRotorType(InternalRotorType type) {
        irType = type;
    }

    public InternalRotorType getInternalRotorType() {
        return irType;
    }
}
