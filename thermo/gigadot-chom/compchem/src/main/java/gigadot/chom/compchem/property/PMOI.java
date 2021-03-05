package gigadot.chom.compchem.property;

import gigadot.chom.compchem.math.ArrayMathUtils;
import gigadot.chom.model.property.CompositeProperty;
import gigadot.chom.model.enumeration.UnitSystem;
import gigatools.lite.constant.PhysicalConstants;
import java.text.DecimalFormat;
import org.apache.commons.lang.StringUtils;

/**
 * Principal Moment of Inertia
 *
 * @author Weerapong Phadungsukanan
 */
public class PMOI extends CompositeProperty {

    private final double[][] paxes = new double[3][3];
    private final double[] pmoi = new double[3];

    /**
     * @deprecated
     */
    public PMOI() {
    }

    /**
     * values are in SI
     *
     * @param pmoi
     * @param paxes first index is the index of the eigen, second is the eigen vector.
     */
    public PMOI(double[] pmoi, double[][] paxes) {
        System.arraycopy(pmoi, 0, this.pmoi, 0, this.pmoi.length);
        for (int i = 0; i < this.paxes.length; i++) {
            System.arraycopy(paxes[i], 0, this.paxes[i], 0, this.paxes[i].length);
        }
    }

    public PMOI(PMOI original) {
        System.arraycopy(original.pmoi, 0, pmoi, 0, pmoi.length);
        for (int i = 0; i < this.paxes.length; i++) {
            System.arraycopy(original.paxes[i], 0, this.paxes[i], 0, this.paxes[i].length);
        }
    }

    /**
     * @param i
     * @param moi_val_InGSD
     * @deprecated
     */
    public void setMOIIn_amu_rbohr2(int i, double moi_val_InGSD) {
        pmoi[i] = moi_val_InGSD * PhysicalConstants.amu * PhysicalConstants.r_Bohr * PhysicalConstants.r_Bohr;
    }

    /**
     * @deprecated @param moi0InGSD
     * @param moi1InGSD
     * @param moi2InGSD
     */
    public void setMOIIn_amu_rbohr2(double moi0InGSD, double moi1InGSD, double moi2InGSD) {
        double f_moi2SI = PhysicalConstants.amu * PhysicalConstants.r_Bohr * PhysicalConstants.r_Bohr;
        pmoi[0] = moi0InGSD * f_moi2SI;
        pmoi[1] = moi1InGSD * f_moi2SI;
        pmoi[2] = moi2InGSD * f_moi2SI;
    }

    /**
     * @deprecated @param i
     * @param x
     * @param y
     * @param z
     */
    public void setPrincipalAxis(int i, double x, double y, double z) {
        paxes[i][0] = x;
        paxes[i][1] = y;
        paxes[i][2] = z;
    }

    public double getMOI(int i) {
        return pmoi[i];
    }

    public double getMOIIn_amu_rbohr2(int i) {
        return pmoi[i] / (PhysicalConstants.amu * PhysicalConstants.r_Bohr * PhysicalConstants.r_Bohr);
    }

    /**
     * get a copy of
     *
     * @return
     */
    public double[] get3ArrayMOI() {
        return ArrayMathUtils.copyOf(pmoi);
    }

    public double[] get3ArrayMOIIn_amu_rbohr2() {
        double f_moi2SI = PhysicalConstants.amu * PhysicalConstants.r_Bohr * PhysicalConstants.r_Bohr;
        return ArrayMathUtils.copyAndScale(pmoi, f_moi2SI);
    }

    public double[] getPrincipalAxis(int i) {
        return ArrayMathUtils.copyOf(paxes[i]);
    }

    @Override
    public String toString(UnitSystem unit) {
        switch (unit) {
            case GSD:
                return formatString(1 / (PhysicalConstants.amu * PhysicalConstants.r_Bohr * PhysicalConstants.r_Bohr), "amu*r_bohr^2");
            case SI:
            default: // SI
                return formatString(1e47, "1E-47 kg*m^2");
        }
    }

    private String formatString(double factor, String unit_str) {
        DecimalFormat dform = new DecimalFormat("#####0.00000");
        return "Principal MOI in (" + unit_str + ") : " + "\n"
                + " Principal axes " + StringUtils.leftPad(dform.format(pmoi[0] * factor), 15) + " " + StringUtils.leftPad(dform.format(pmoi[1] * factor), 15) + " " + StringUtils.leftPad(dform.format(pmoi[2] * factor), 15) + "\n"
                + "        X       " + StringUtils.leftPad(dform.format(paxes[0][0]), 15) + " " + StringUtils.leftPad(dform.format(paxes[1][0]), 15) + " " + StringUtils.leftPad(dform.format(paxes[2][0]), 15) + "\n"
                + "        Y       " + StringUtils.leftPad(dform.format(paxes[0][1]), 15) + " " + StringUtils.leftPad(dform.format(paxes[1][1]), 15) + " " + StringUtils.leftPad(dform.format(paxes[2][1]), 15) + "\n"
                + "        Z       " + StringUtils.leftPad(dform.format(paxes[0][2]), 15) + " " + StringUtils.leftPad(dform.format(paxes[1][2]), 15) + " " + StringUtils.leftPad(dform.format(paxes[2][2]), 15) + "\n";
    }
}
