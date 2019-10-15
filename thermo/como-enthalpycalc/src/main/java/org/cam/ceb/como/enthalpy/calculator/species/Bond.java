/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.species;

/**
 *
 * @author pb556
 */
public class Bond<T> {

    protected BondType bondType = BondType.SINGLE;
    protected T refAtomA = null;
    protected T refAtomB = null;

    public Bond(BondType bondType, T refAtomA, T refAtomB) {
        this.bondType = bondType;
        if (refAtomA.toString().compareTo(refAtomB.toString()) < 0) {
            this.refAtomA = refAtomA;
            this.refAtomB = refAtomB;
        } else {
            this.refAtomA = refAtomB;
            this.refAtomB = refAtomA;
        }
    }

    public T getRefAtomA() {
        return refAtomA;
    }

    public T getRefAtomB() {
        return refAtomB;
    }

    public BondType getBondType() {
        return bondType;
    }

    public boolean contains(T ref) {
        return ref.equals(refAtomA) || ref.equals(refAtomB);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Bond) {
            Bond b = (Bond) obj;
            return b.getBondType() == bondType
                    && b.getRefAtomA().equals(refAtomA)
                    && b.getRefAtomB().equals(refAtomB);
        }
        return false;
    }

    @Override
    public String toString() {
        final String representationTemplate = "[%s]{%s}[%s]";
        if (refAtomA.toString().compareTo(refAtomB.toString()) < 0) {
            return String.format(representationTemplate, refAtomA.toString(), bondType.getValue(), refAtomB.toString());
        } else {
            return String.format(representationTemplate, refAtomB.toString(), bondType.getValue(), refAtomA.toString());
        }
    }
    
    @Override
    public Bond clone() {
        return new Bond(bondType, refAtomA, refAtomB);
    }
}
