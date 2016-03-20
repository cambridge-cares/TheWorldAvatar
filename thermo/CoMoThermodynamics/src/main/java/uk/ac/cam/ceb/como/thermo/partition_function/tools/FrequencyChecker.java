package uk.ac.cam.ceb.como.thermo.partition_function.tools;

import uk.ac.cam.ceb.como.chem.info.alias.ThermoAnalyzable;
import uk.ac.cam.ceb.como.chem.property.Displacement;
import uk.ac.cam.ceb.como.chem.property.Vibration;
import uk.ac.cam.ceb.como.chem.structure.util.StructureTools;
import uk.ac.cam.ceb.como.compchem.property.PMOI;
import uk.ac.cam.ceb.como.math.linear_algebra.VectorUtil;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class FrequencyChecker {

    private static Logger logger = Logger.getLogger(FrequencyChecker.class);

    public static boolean isNumberOfFrequenciesCorrect(ThermoAnalyzable compound) {
        // if linear nf = 3*n - 5
        if (!StructureTools.isSingleAtom(compound)) {
            boolean isLinear = StructureTools.isLinear(compound);
            if (isLinear) {
                return compound.getVibrationCount() == (3 * compound.getAtomCount() - 5);
            } else {
                return compound.getVibrationCount() == (3 * compound.getAtomCount() - 6);
            }
        } else {
            return compound.getVibrationCount() == 0;
        }
    }



    public static void ensureCorrectFrequencyModes(ThermoAnalyzable structure) {
        if (!StructureTools.isSingleAtom(structure)) {
            boolean isLinear = StructureTools.isLinear(structure);
            if (isLinear && (structure.getVibrationCount() == (3 * structure.getAtomCount() - 6))) {
                // double frequency
                // jDoc get axis with zero moi
                PMOI pmoi = structure.getPrincipalMOI();
                double[] pdmoi = pmoi.get3ArrayMOI();
                int index = -1;
                for (int i = 0; i < pdmoi.length; i++) {
                    if (StructureTools.isMOIZero(pdmoi[i])) {
                        index = i;
                        break;
                    }
                }
                double[] axis = pmoi.getPrincipalAxis(index);
                double fdotmin = Double.MAX_VALUE;
                int fdotmin_index = -1;
                for (int i = 0; i < structure.getVibrationCount(); i++) {
                    // sum over all eigens
                    double[] sum_eigen = VectorUtil.createZeroVector();
                    for (int j = 0; j < structure.getVibration(i).getDisplacements().size(); j++) {
                        Displacement disp = structure.getVibration(i).getDisplacements().get(j);
                        sum_eigen[0] += disp.dxInA();
                        sum_eigen[1] += disp.dyInA();
                        sum_eigen[2] += disp.dzInA();
                    }
                    // dot axis with sum
                    double axis_dot_sume = VectorUtil.getDotProduct(axis, sum_eigen);
                    // loop for all frequency find the min index
                    if (Math.abs(axis_dot_sume) < fdotmin) {
                        fdotmin = axis_dot_sume;
                        fdotmin_index = i;
                    }
                }
                // for min f cross with axis for all eigen
                Vibration fdata = new Vibration();
                Vibration existing_fdata = structure.getVibration(fdotmin_index);
                fdata.setForceConstant(existing_fdata.getForceConstant());
                fdata.setIRInten(existing_fdata.getIRInten());
                fdata.setFrequency(existing_fdata.getFrequency());
                fdata.setReducedMass(existing_fdata.getReducedMass());
                for (int i = 0; i < existing_fdata.getDisplacements().size(); i++) {
                    Displacement odisp = existing_fdata.getDisplacements().get(i);
                    double[] dodisp = VectorUtil.createVector(odisp.dxInA(), odisp.dyInA(), odisp.dzInA());
                    Displacement ndisp = new Displacement();
                    double[] cross = new double[3];
                    VectorUtil.getCrossProduct(axis, dodisp, cross);
                    ndisp.setdxdydzInA(cross[0], cross[1], cross[2]);
                    fdata.getDisplacements().add(ndisp);
                }
                structure.insertVibration(fdata, 0);
                logger.warn("Number of frequencies not matched. A frequency (index = " + fdotmin_index + ") is added : f = " + fdata.getFrequency());
            } else if (isLinear && (structure.getVibrationCount() == (3 * structure.getAtomCount() - 5))) {
            } else if (structure.getVibrationCount() == (3 * structure.getAtomCount() - 6)) {
            } else {
                throw new RuntimeException("This is not a linear molecule and the number frequecies do not match");
            }
        }
    }
}
