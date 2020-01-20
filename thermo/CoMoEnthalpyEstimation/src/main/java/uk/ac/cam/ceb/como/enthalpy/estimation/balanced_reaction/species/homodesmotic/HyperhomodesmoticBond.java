/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.homodesmotic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.BondType;

/**
 *
 * @author pb556
 */
public class HyperhomodesmoticBond {
    
    protected BondType bondType;
    protected HyperhomodesmoticAtom atomA;
    protected HyperhomodesmoticAtom atomB;
    
    // the same element can be used multiple times
    protected Map<HyperhomodesmoticAtom, BondType> bondsAtomA;
    protected Map<HyperhomodesmoticAtom, BondType> bondsAtomB;

    public HyperhomodesmoticBond(BondType bondType, 
            HyperhomodesmoticAtom atomA, 
            HyperhomodesmoticAtom atomB, 
            Map<HyperhomodesmoticAtom, BondType> bondsAtomA, 
            Map<HyperhomodesmoticAtom, BondType> bondsAtomB) {
        this.bondType = bondType;
        this.atomA = atomA;
        this.atomB = atomB;
        this.bondsAtomA = bondsAtomA;
        this.bondsAtomB = bondsAtomB;
    }

    public BondType getBondType() {
        return bondType;
    }

    public HyperhomodesmoticAtom getAtomA() {
        return atomA;
    }
    
    public HyperhomodesmoticAtom getAtomB() {
        return atomB;
    }
    
    public Map<HyperhomodesmoticAtom, BondType> getBondsA() {
        return bondsAtomA;
    }
    
    public Map<HyperhomodesmoticAtom, BondType> getBondsB() {
        return bondsAtomB;
    }

    public boolean equals(HyperhomodesmoticBond bond) {
        if (atomA.equals(bond.getAtomA()) && atomB.equals(bond.getAtomB()) && bondType == bond.getBondType()) {
            if (bondsAtomA.size() != bond.getBondsA().size() || bondsAtomB.size() != bond.getBondsB().size()) {
                return false;
            }
            for (HyperhomodesmoticAtom atom1 : bondsAtomA.keySet()) {
                boolean identified = false;
                for (HyperhomodesmoticAtom atom2 : bond.getBondsA().keySet()) {
                    if (atom1.equals(atom2) && bondsAtomA.get(atom1) == bond.getBondsA().get(atom2)) {
                        identified = true;
                        break;
                    }
                }
                if (!identified) {
                    return false;
                }
            }
            for (HyperhomodesmoticAtom atom1 : bondsAtomB.keySet()) {
                boolean identified = false;
                for (HyperhomodesmoticAtom atom2 : bond.getBondsB().keySet()) {
                    if (atom1.equals(atom2) && bondsAtomB.get(atom1) == bond.getBondsB().get(atom2)) {
                        identified = true;
                        break;
                    }
                }
                if (!identified) {
                    return false;
                }
            }
        }
        if (atomB.equals(bond.getAtomA()) && atomA.equals(bond.getAtomB()) && bondType == bond.getBondType()) {
            if (bondsAtomB.size() != bond.getBondsA().size() || bondsAtomA.size() != bond.getBondsB().size()) {
                return false;
            }
            for (HyperhomodesmoticAtom atom1 : bondsAtomB.keySet()) {
                boolean identified = false;
                for (HyperhomodesmoticAtom atom2 : bond.getBondsA().keySet()) {
                    if (atom1.equals(atom2) && bondsAtomB.get(atom1) == bond.getBondsA().get(atom2)) {
                        identified = true;
                        break;
                    }
                }
                if (!identified) {
                    return false;
                }
            }
            for (HyperhomodesmoticAtom atom1 : bondsAtomA.keySet()) {
                boolean identified = false;
                for (HyperhomodesmoticAtom atom2 : bond.getBondsB().keySet()) {
                    if (atom1.equals(atom2) && bondsAtomA.get(atom1) == bond.getBondsB().get(atom2)) {
                        identified = true;
                        break;
                    }
                }
                if (!identified) {
                    return false;
                }
            }
        }
        return false;
    }
    
    @Override
    public String toString() {
        ArrayList<String> bondA = getSortedBondsString(bondsAtomA);
        ArrayList<String> bondB = getSortedBondsString(bondsAtomB);
        String strBondA = "";
        for (String b : bondA) {
            strBondA += b;
        }
        String strBondB = "";
        for (String b : bondB) {
            strBondB += b;
        }
        ArrayList<String> bonds = new ArrayList<String>();
        bonds.add(strBondA);
        bonds.add(strBondB);
        Collections.sort(bonds);
        if (bonds.get(0).equals(strBondA)) {
            return bonds.get(0) + "[" + atomA.getElement().getSymbol() + "]{" + bondType.getValue() + "}[" + atomB.getElement().getSymbol() + "]" + bonds.get(1);
        } else {
            return bonds.get(0) + "[" + atomB.getElement().getSymbol() + "]{" + bondType.getValue() + "}[" + atomA.getElement().getSymbol() + "]" + bonds.get(1);
        }
    }
    
    protected ArrayList<String> getSortedBondsString(Map<HyperhomodesmoticAtom, BondType> bonds) {
        ArrayList<String> b = new ArrayList<String>();
        for (HyperhomodesmoticAtom atom : bonds.keySet()) {
            b.add("([" + atom.getElement().getSymbol() + "]{" + bonds.get(atom).getValue() + "})");
        }
        Collections.sort(b);
        return b;
    }
}
