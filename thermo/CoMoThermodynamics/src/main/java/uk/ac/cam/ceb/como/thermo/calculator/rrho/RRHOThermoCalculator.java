/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.calculator.rrho;

import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.chem.property.Rotation;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.IRStatMechThermoCalculator;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunctions;

/**
 *
 * @author pb556
 */
public class RRHOThermoCalculator extends IRStatMechThermoCalculator {

    protected Logger logger = Logger.getLogger(getClass());

    public RRHOThermoCalculator() {
        super();
    }

    @Override
    protected void applyChangesToPartionFunction() throws Exception {
        PartitionFunctions Q = getQ();
        if (Q == null || recalculationNeeded()) {
            Q = new PartitionFunctions();
            // Translational Partition Function.
            if (hasTranslation()) {
                Q.add(getTranslationalParitionFunction());
            }

            // Electronic Partition Function.
            if (hasElectronic()) {
                Q.add(getElectronicParitionFunction());
            }

            // Rotational Partition Function.
            if (hasRotation()) {
                Q.add(getRotationalPartitionFunction());
            }

            if (hasInternalRotation()) {
                getInternalRotorRule().setThermoAnalyzable(getThermoAnalyzable());
            }

            // Vibrational Partition Function.
            for (int i = 0; i < getThermoAnalyzable().getVibrationCount(); i++) {
                if (hasVibration()) {
                    Q.add(getVibrationalPartitionFunction(getThermoAnalyzable().getVibration(i).getFrequency()));
                }
            }
            recalculation(false);
            set(Q);
            setH_0K(calculateH_0K());
        }
    }
}
