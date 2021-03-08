package gigadot.chom.compchem.info;

import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemWrapper;
import gigadot.chom.compchem.orm.annotation.CompChemWrapperField;
import gigadot.chom.compchem.orm.annotation.ContainerType;
import gigadot.chom.compchem.orm.annotation.Prefixes;
import gigadot.chom.compchem.orm.annotation.Property;
import gigadot.chom.compchem.property.PMOI;
import gigadot.chom.compchem.property.RotationalConstants;
import gigatools.lite.constant.PhysicalConstants;

/**
 * this class if for testing. don't use it.
 * @author wp214
 */
@Prefixes({"cc : " + CompChem.COMPCHEM_NS, "nonSi : " + CompChem.NONSI_NS})
public class ComputedTargetInfo implements ComputedInfo {

    @CompChemWrapperField
    private CompChemWrapper ccw;
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
    @Property(dictRef = "cc:scfenergy", units = "nonSi:hartree")
    public double getFinalSCFEnergyInHartree() {
        return finalSCFEnergyInHartree;
    }

    @Override
    public void setFinalSCFEnergyInHartree(double SCFEnergyInHartree) {
        this.finalSCFEnergyInHartree = SCFEnergyInHartree;
    }

    public double[] getDBs() {
        return new double[]{1d, 2.5, 4.0};
    }

    @Property(dictRef = "cc:dbs", units = "si:none", type = ContainerType.Array, optional = true)
    public void setDBs(double[] dbs) {
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
