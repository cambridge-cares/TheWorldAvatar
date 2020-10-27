package gigadot.chom.compchem.info;

import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.compchem.property.RotationalConstants;
import gigatools.lite.constant.PhysicalConstants;

/**
 *
 * @author wp214
 */
public class ComputedInfoImpl implements ComputedInfo {

    private double finalSCFEnergyInHartree = 0.0;
    // New fields in wml version 0b
    private PMOI principalMOI = null;
    // New fields in wml version 0d
    private RotationalConstants rotConst = null;

    @Override
    public void clear() {
        finalSCFEnergyInHartree = 0.0;
        principalMOI = null;
        rotConst = null;
    }

    @Override
    public double getFinalSCFEnergyInHartree() {
        return finalSCFEnergyInHartree;
    }

    @Override
    public void setFinalSCFEnergyInHartree(double SCFEnergyInHartree) {
        this.finalSCFEnergyInHartree = SCFEnergyInHartree;
    }

    /**
     * set in J/mol
     * @param SCFEnergy
     */
    @Override
    public void setFinalSCFEnergy(double SCFEnergy) {
        finalSCFEnergyInHartree = SCFEnergy / PhysicalConstants.Hartree;
    }

    /**
     * get in J/mol
     * @return
     */
    @Override
    public double getFinalSCFEnergy() {
        return finalSCFEnergyInHartree * PhysicalConstants.Hartree;
    }

    /**
     * Get a clone of {@link PrincipalMOI} object.
     * @return A clone of {@link PrincipalMOI} object.
     */
    @Override
    public PMOI getPrincipalMOI() {
        if (principalMOI != null) {
            return new PMOI(principalMOI);
        }
        return null;
    }

    /**
     * Set {@link PrincipalMOI} object by making a clone.
     * @param moi A {@link PrincipalMOI} object.
     */
    @Override
    public void setPricipleMOI(PMOI moi) {
        this.principalMOI = new PMOI(moi);
    }

    @Override
    public RotationalConstants getRotationalConstants() {
        if (rotConst != null) {
            return new RotationalConstants(rotConst);
        }
        return null;
    }

    @Override
    public void setRotationalConstants(RotationalConstants rotConst) {
        this.rotConst = new RotationalConstants(rotConst);
    }
}
