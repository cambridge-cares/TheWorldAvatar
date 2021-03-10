package uk.ac.cam.ceb.como.thermo.partition_function.rotation.overall;

import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.compchem.property.PMOI;
import uk.ac.cam.ceb.como.chem.moi.MOICalculator;
import uk.ac.cam.ceb.como.chem.structure.util.StructureTools;
import uk.ac.cam.ceb.como.math.constant.PhysicalConstants;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionFunction;
import uk.ac.cam.ceb.como.thermo.partition_function.PartitionValues;

/**
 *
 * @author pb556
 */
public class RotationalPartionFunction extends PartitionFunction {

    private boolean isLinear = false;
    private boolean isSingleAtom = false;
    private double[] pI = null;
    private int n_symmetry = 1;
    private double I = 0.0;
    private double T = 0.0;

    @Override
    public void setThermoAnalyzable(ThermoAnalyzable thermoAnalyzable) {
        super.setThermoAnalyzable(thermoAnalyzable);

        MOICalculator mc = new MOICalculator(thermoAnalyzable);
        PMOI pmoi = mc.calculatePrincipalMOI();
        pI = pmoi.get3ArrayMOI();

        if (isSingleAtom = StructureTools.isSingleAtom(thermoAnalyzable)) {
            pI[0] = pI[1] = pI[2] = 0.0;
        } else if (isLinear = StructureTools.isLinear(thermoAnalyzable)) {
            // If it is linear then the other two principal MOIs are equal. Even if
            // it is an atom this is still valid.
            I = (pI[0] + pI[1] + pI[2]) / 2.0;
        }

        n_symmetry = thermoAnalyzable.getRotationalSymmetryNumber();
    }

    @Override
    public PartitionValues getPartitionValues(double T) {
        this.T = T;
        PartitionValues Q = new PartitionValues();
        double k_B = PhysicalConstants.k_B;
        double hbar = PhysicalConstants.hbar;
        if (isSingleAtom) {
            // CALCULATE q
            Q.q = 1.0;
            // CALCULATE dqBydT
            Q.dqBydT = 0.0;
            // CALCULATE d2qBydT2
            Q.d2qBydT2 = 0.0;
        } else if (isLinear) {
            Q.q = 2.0 * (I / hbar) * (k_B / hbar) * T / n_symmetry;
            // CALCULATE dqBydT
            Q.dqBydT = Q.q / T;
            // CALCULATE d2qBydT2
            Q.d2qBydT2 = 0.0;
        } else {
            double t_factor = 2 * k_B / hbar / hbar;
            double pre_fact = Math.sqrt(Math.PI
                    * t_factor * pI[0]
                    * t_factor * pI[1]
                    * t_factor * pI[2]);
            // CALCULATE q
            Q.q = pre_fact * Math.pow(T, 1.5) / n_symmetry;
            // CALCULATE dqBydT
            Q.dqBydT = 1.5 * Q.q / T;
            // CALCULATE d2qBydT2
            Q.d2qBydT2 = 0.75 * Q.q / T / T;
        }
        
        //System.out.println("qrot = " + Q.q);
        
        return Q;
    }
}
