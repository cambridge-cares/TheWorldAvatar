package gigadot.chom.compchem.property;

import gigadot.chom.compchem.math.ArrayMathUtils;
import gigadot.chom.model.property.CompositeProperty;
import gigadot.chom.model.enumeration.UnitSystem;
import gigatools.lite.constant.PhysicalConstants;

/**
 * need tests
 *
 * @author Weerapong Phadungsukanan
 */
public class RotationalConstants extends CompositeProperty {

    private final double[] rot_constants = new double[3];

    public RotationalConstants(double[] rot_consts) {
        ArrayMathUtils.copy(rot_consts, rot_constants);
    }

    public RotationalConstants(RotationalConstants rotc) {
        System.arraycopy(rotc.rot_constants, 0, this.rot_constants, 0, this.rot_constants.length);
    }

    public static RotationalConstants from_GHz(double[] rInGHz) {
        double[] rcs = ArrayMathUtils.copyAndScale(rInGHz, 1e9);
        return new RotationalConstants(rcs);
    }

    public static RotationalConstants fromPMOI(double[] pmoi) {
        double f_conv = PhysicalConstants.hbar / 4 / Math.PI;
        double[] rcs = ArrayMathUtils.copyOf(new double[]{f_conv / pmoi[0], f_conv / pmoi[1], f_conv / pmoi[2]});
        return new RotationalConstants(rcs);
    }

    public static RotationalConstants fromPMOIIn_amu_rbohr2(double[] pmoiInGSD) {
        double f_moi2SI = PhysicalConstants.amu * PhysicalConstants.r_Bohr * PhysicalConstants.r_Bohr;
        double f_conv = PhysicalConstants.hbar / 4 / Math.PI;
        double[] rcs = ArrayMathUtils.copyAndScale(new double[]{1d / pmoiInGSD[0], 1d / pmoiInGSD[1], 1d / pmoiInGSD[2]}, f_conv / f_moi2SI);
        return new RotationalConstants(rcs);

    }

    public double getValue(int i) {
        return rot_constants[i];
    }

    public double getValueInGHz(int i) {
        return rot_constants[i] / 1e9;
    }

    @Override
    public String toString(UnitSystem unit) {
        return "Rotational constants (GHz) : " + getValueInGHz(0) + ", " + getValueInGHz(1) + ", " + getValueInGHz(2);
    }
}
