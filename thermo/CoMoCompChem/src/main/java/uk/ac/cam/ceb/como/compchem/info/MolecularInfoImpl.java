package uk.ac.cam.ceb.como.compchem.info;

/**
 *
 * @author pb556
 */

public class MolecularInfoImpl implements MolecularInfo {

    private int charge = 0;
    private int multiplicity = 1;
    private int rotationalSymmetryNumber = 1;

    @Override
    public void clear() {
        charge = 0;
        multiplicity = 1;
        rotationalSymmetryNumber = 1;
    }

    @Override
    public int getSpinMultiplicity() {
        return multiplicity;
    }

    @Override
    public void setMultiplicity(int multiplicity) {
        this.multiplicity = multiplicity;
    }

    /**
     * Get molecular charge in integer as multiple of <code>abs(e)</code>, where e is
     * the positive value of electron charge.
     *
     * @return Molecular charge in integer as multiple of <code>abs(e)</code>.
     */
    @Override
    public int getCharge() {
        return charge;
    }

    /**
     * Set molecular charge in integer as multiple of <code>abs(e)</code>, where e is
     * the positive value of electron charge.
     *
     * @param charge Molecular charge in integer as multiple of <code>abs(e)</code>.
     */
    @Override
    public void setCharge(int charge) {
        this.charge = charge;
    }

    @Override
    public int getRotationalSymmetryNumber() {
        return rotationalSymmetryNumber;
    }

    @Override
    public void setRotationalSymmetryNumber(int rotationalSymmetryNumber) {
        this.rotationalSymmetryNumber = rotationalSymmetryNumber;
    }
}
