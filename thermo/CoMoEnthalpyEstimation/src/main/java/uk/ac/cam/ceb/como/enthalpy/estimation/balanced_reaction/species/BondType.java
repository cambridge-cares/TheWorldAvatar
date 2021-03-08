package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species;

/**
 * Enumeration of supported bond types, i.e. single, double, triple and aromatic
 * bonds.
 *
 * @author pb556
 */
public enum BondType {

    /**
     * Single bond.
     */
    SINGLE(1),
    /**
     * Double bond.
     */
    DOUBLE(2),
    /**
     * Triple bond.
     */
    TRIPLE(3),
    /**
     * Aromatic bond.
     */
    AROMATIC(4);
    private final int value;

    BondType(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }
}
