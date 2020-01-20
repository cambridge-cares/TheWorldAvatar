/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.hindered.c1dhr;

import java.util.ArrayList;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.chem.property.Rotation;
import uk.ac.cam.ceb.como.chem.structure.Bond;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.IRStatMechThermoCalculator;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunctions;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.c1dhr.C1DHRIredPartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.rule.PitzerReducedMOIFunctionAngleDependent;

/**
 *
 * @author pb556
 */

public class C1DHRIredThermoCalculator extends IRStatMechThermoCalculator {

    protected Logger logger = Logger.getLogger(getClass());

    public C1DHRIredThermoCalculator() {
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
                                C1DHRIredPartitionFunction q_chind = new C1DHRIredPartitionFunction(getHindrancePotential(rBond));
                                //q_chind.setReducedMoment(getInternalRotorRule().getReducedMOI(rBond, getThermoAnalyzable().getVibrations().clonedRotorBonds()));
                                q_chind.setReducedMoment(new PitzerReducedMOIFunctionAngleDependent(getInternalRotorRule(), rBond, getThermoAnalyzable().getVibrations().clonedRotorBonds()));
                                q_chind.setRotorSymmetry(getInternalRotorRule().getRotorSymmetry(rBond));
                                q_chind.setThermoAnalyzable(getThermoAnalyzable());
                                Q.add(q_chind);
                                break;
                            default:
                                if (hasVibration()) {
                                    Q.add(getVibrationalPartitionFunction(getThermoAnalyzable().getVibration(i).getFrequency()));
                                }
                                break;
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
