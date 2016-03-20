package uk.ac.cam.ceb.como.compchem.info;

/**
 *
 * @author pb556
 */
public interface MolecularInfo extends Info {

    /**
     * Get molecular charge in integer as multiple of <code>abs(e)</code>, where e is
     * the positive value of electron charge.
     *
     * @return Molecular charge in integer as multiple of <code>abs(e)</code>.
     */
    int getCharge();

    int getRotationalSymmetryNumber();

    int getSpinMultiplicity();

    /**
     * Set molecular charge in integer as multiple of <code>abs(e)</code>, where e is
     * the positive value of electron charge.
     *
     * @param charge Molecular charge in integer as multiple of <code>abs(e)</code>.
     */
    void setCharge(int charge);

    void setMultiplicity(int multiplicity);

    void setRotationalSymmetryNumber(int rotationalSymmetryNumber);
    
}
