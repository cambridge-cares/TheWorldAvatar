package gigadot.chom.chem.structure;

import gigadot.chom.model.brownie.Atom;
import gigatools.lite.constant.PhysicalConstants;
import org.apache.commons.math.util.MathUtils;

/**
 *
 * @author Weerapong Phadungsukanan
 */
public class Bond {

    private Atom pAtomA = null;
    private Atom pAtomB = null;
    // private int pBondType = 0;
    private BondType pBondType = BondType.UNKNOWN;
    /**
     * Torsional potential barrier height in J/mol
     */
    private double TorsionBarrier = 0.0;

    public static Bond createBond(Atom atomA, Atom atomB, BondType type) {
        return new Bond(atomA, atomB, type);
    }

    public static Bond createBond(Atom atomA, Atom atomB, BondType type, Structure structure) {
        return structure.addBond(new Bond(atomA, atomB, type));
    }

    public void setTorsionBarrierInkcal_Per_mol(double torsionEInkcal_Per_mol) {
        this.TorsionBarrier = torsionEInkcal_Per_mol * PhysicalConstants.kCal;
    }

    public double getTorsionBarrierInkcal_Per_mol() {
        return TorsionBarrier / PhysicalConstants.kCal;
    }

    public void setTorsionBarrier(double torsionE) {
        this.TorsionBarrier = torsionE;
    }

    public double getTorsionBarrier() {
        return TorsionBarrier;
    }

    /**
     * Get bond length.
     * @return
     * @throws NullPointerException if either atom is null.
     */
    public double getBondLength() {
        double[] p1 = {pAtomA.getX(), pAtomA.getY(), pAtomA.getZ()};
        double[] p2 = {pAtomB.getX(), pAtomB.getY(), pAtomB.getZ()};
        return MathUtils.distance(p1, p2);
    }

    public double getBondLengthInA() {
        double[] p1 = {pAtomA.getXInA(), pAtomA.getYInA(), pAtomA.getZInA()};
        double[] p2 = {pAtomB.getXInA(), pAtomB.getYInA(), pAtomB.getZInA()};
        return MathUtils.distance(p1, p2);
    }

    private Bond(Atom atomA, Atom atomB, BondType type) {
        pAtomA = atomA;
        pAtomB = atomB;
        pBondType = type;
    }

    public Atom getAtomA() {
        return pAtomA;
    }

    public Atom getAtomB() {
        return pAtomB;
    }

    public BondType getBondType() {
        return pBondType;
    }

    public boolean hasAtom(Atom atom) {
        return (atom == pAtomA) || (atom == pAtomB);
    }

    public boolean isBetween(String symbol1, String symbol2) {
        if (pAtomA == null || pAtomB == null) {
            return false;
        }
        return ((pAtomA.is(symbol1) && pAtomB.is(symbol2))
                || (pAtomA.is(symbol2) && pAtomB.is(symbol1)));
    }

    public boolean isBetween(Atom atom, String symbol2) {
        if (pAtomA == null || pAtomB == null) {
            return false;
        }
        return ((pAtomA.equalsIgnoreOrder(atom) && pAtomB.is(symbol2))
                || (pAtomA.is(symbol2) && pAtomB.equalsIgnoreOrder(atom)));
    }

    public boolean isBetween(String symbol1, String symbol2, BondType type) {
        return isBetween(symbol1, symbol2) && (pBondType == type);
    }

    public boolean isBetween(Atom atom, String symbol2, BondType type) {
        return isBetween(atom, symbol2) && (pBondType == type);
    }

    public boolean isSimilar(Bond bond) {
        if (this == bond) {
            return true;
        }
        if ((bond == null) || (bond.getClass() != this.getClass())) {
            return false;
        }
        // Object must be Bond at this point
        boolean iHaveSimilarEnds = ((this.pAtomA.getOrder() == bond.pAtomA.getOrder())
                && (this.pAtomB.getOrder() == bond.pAtomB.getOrder()))
                || ((this.pAtomA.getOrder() == bond.pAtomB.getOrder())
                && (this.pAtomB.getOrder() == bond.pAtomA.getOrder()));
        return iHaveSimilarEnds
                && (this.pBondType == bond.pBondType);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if ((obj == null) || (obj.getClass() != this.getClass())) {
            return false;
        }
        // Object must be Bond at this point
        Bond bond = (Bond) obj;
        boolean iHaveSameEnds = ((this.pAtomA.equalsIgnoreOrder(bond.pAtomA))
                && (this.pAtomB.equalsIgnoreOrder(bond.pAtomB)))
                || ((this.pAtomA.equalsIgnoreOrder(bond.pAtomB))
                && (this.pAtomB.equalsIgnoreOrder(bond.pAtomA)));
        return iHaveSameEnds
                && (this.pBondType == bond.pBondType);
    }

    @Override
    public int hashCode() {
        int hash = 3;
        //hash = 59 * hash + (this.mDoc != null ? this.mDoc.hashCode() : 0);
        hash = 59 * hash + (this.pAtomA != null ? this.pAtomA.hashCode() : 0);
        hash = 59 * hash + (this.pAtomB != null ? this.pAtomB.hashCode() : 0);
        hash = 59 * hash + this.pBondType.ordinal();
        return hash;
    }

    @Override
    public String toString() {
        return "R(" + pAtomA.getOrder() + ", " + pAtomB.getOrder() + ") Type : " + pBondType;
    }
}
