/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.hindered.s1dhr;

import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.chem.property.Rotation;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.IRStatMechThermoCalculator;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunctions;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.s1dhr.S1DHRPartitionFunction;

/**
 *
 * @author pb556
 */
public class S1DHRThermoCalculator extends IRStatMechThermoCalculator {

    protected Logger logger = Logger.getLogger(getClass());

    public S1DHRThermoCalculator() {
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

            // Vibrational and Internal Rotation Partition Function.
            for (int i = 0; i < getThermoAnalyzable().getVibrationCount(); i++) {
                if ((getThermoAnalyzable().getVibration(i)) instanceof Rotation) {
                    if (getInternalRotorRule() == null || !hasInternalRotation()) {
                        if (hasVibration()) {
                            Q.add(getVibrationalPartitionFunction(getThermoAnalyzable().getVibration(i).getFrequency()));
                        }
                    } else {
                        Rotation rot = (Rotation) getThermoAnalyzable().getVibration(i);
                        if (rot.getSymmetryNumber() > 1) {
                            Bond rBond = rot.getTorsionalBond();
                            // Rotor rule and rotor calculator
                            getInternalRotorRule().setThermoAnalyzable(getThermoAnalyzable());
                            // Check the internal rotor type
                            switch (getInternalRotorRule().getInternalRotorType()) {
                                case FREE_ROTOR:
                                    if (!hasInternalRotation()) {
                                        break;
                                    }
                                    Q.add(getFreeRotorPartitionFunction(rBond, getInternalRotorRule().getReducedMOI(rBond, getThermoAnalyzable().getVibrations().clonedRotorBonds()),
                                            getInternalRotorRule().getRotorSymmetry(rBond)));
                                    break;
                                case CLASSICAL_LIMIT_HINDERED_ROTOR:
                                    if (!hasInternalRotation()) {
                                        break;
                                    }
                                    //use different partition function rule here!
                                    S1DHRPartitionFunction q_chind = new S1DHRPartitionFunction();
                                    q_chind.setBarrierEnergy(rot.getBarrierHeight());
                                    q_chind.setReducedMoment(rot.getReducedMoment(), rot.getSymmetryNumber());
                                    q_chind.setThermoAnalyzable(getThermoAnalyzable());
                                    Q.add(q_chind);
                                    break;
                                default:
                                    if (hasVibration()) {
                                        Q.add(getVibrationalPartitionFunction(getThermoAnalyzable().getVibration(i).getFrequency()));
                                    }
                                    break;
                            }
                        } else {
                            if (hasVibration()) {
                                Q.add(getVibrationalPartitionFunction(getThermoAnalyzable().getVibration(i).getFrequency()));
                            }
                        }
                    }
                } else {
                    if (hasVibration()) {
                        Q.add(getVibrationalPartitionFunction(getThermoAnalyzable().getVibration(i).getFrequency()));
                    }
                }
            }
            recalculation(false);
            set(Q);
            setH_0K(calculateH_0K());
        }
    }
}
