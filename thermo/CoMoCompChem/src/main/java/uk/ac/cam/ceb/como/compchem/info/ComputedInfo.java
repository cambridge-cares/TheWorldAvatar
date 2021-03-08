package uk.ac.cam.ceb.como.compchem.info;

import uk.ac.cam.ceb.como.compchem.property.PMOI;
import uk.ac.cam.ceb.como.compchem.property.RotationalConstants;

/**
 *
 * @author pb556
 */
public interface ComputedInfo extends Info {

    /**
     * get in J/mol
     * @return
     */
    double getFinalSCFEnergy();

    double getFinalSCFEnergyInHartree();

    /**
     * Get a clone of {@link PrincipalMOI} object.
     * @return A clone of {@link PrincipalMOI} object.
     */
    PMOI getPrincipalMOI();

    RotationalConstants getRotationalConstants();

    /**
     * set in J/mol
     * @param SCFEnergy
     */
    void setFinalSCFEnergy(double SCFEnergy);

    void setFinalSCFEnergyInHartree(double SCFEnergyInHartree);

    /**
     * Set {@link PrincipalMOI} object by making a clone.
     * @param moi A {@link PrincipalMOI} object.
     */
    void setPricipleMOI(PMOI moi);

    void setRotationalConstants(RotationalConstants rotConst);
    
}