/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.partition_function.electronic;

import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;

/**
 *
 * @author pb556
 */
public class ElectronicPartitionFunction extends PartitionFunction {

    private double T;
    
    @Override
    public void setThermoAnalyzable(ThermoAnalyzable mDoc) {
        super.setThermoAnalyzable(mDoc);
    }

    @Override
    public PartitionValues getPartitionValues(double T) {
        this.T = T;
        PartitionValues Q = new PartitionValues();
        
        // CALCULATE q
        Q.q = thermoAnalyzable.getSpinMultiplicity();

        // CALCULATE dqBydT
        Q.dqBydT = 0.0;

        // CALCULATE d2qBydT2
        Q.d2qBydT2 = 0.0;

        return Q;
    }
}
