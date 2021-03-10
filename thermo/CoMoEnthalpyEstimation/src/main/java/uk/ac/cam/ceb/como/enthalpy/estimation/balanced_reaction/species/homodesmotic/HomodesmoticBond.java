/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.homodesmotic;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.BondType;
import uk.ac.cam.ceb.como.chem.periodictable.Element;

/**
 *
 * @author pb556
 */
public class HomodesmoticBond {

    protected BondType bondType;
    protected Element elementA;
    protected Element elementB;
    protected int numAddBondsA;
    protected int numAddBondsB;

    public HomodesmoticBond(BondType bondType, Element elementA, Element elementB, int numAddBondsA, int numAddBondsB) {
        this.bondType = bondType;
        this.elementA = elementA;
        this.elementB = elementB;
        this.numAddBondsA = numAddBondsA;
        this.numAddBondsB = numAddBondsB;
    }

    public BondType getBondType() {
        return bondType;
    }

    public Element getElementA() {
        return elementA;
    }
    
    public Element getElementB() {
        return elementB;
    }
    
    public int getNumAddBondsA() {
        return numAddBondsA;
    }
    
    public int getNumAddBondsB() {
        return numAddBondsB;
    }

    public boolean equals(HomodesmoticBond bond) {
        if (bond.getElementA().getSymbol().equals(getElementA().getSymbol()) && 
                bond.getElementB().getSymbol().equals(getElementB().getSymbol())) {
            return bond.getBondType() == bondType &&
                getNumAddBondsA() == bond.getNumAddBondsA() && getNumAddBondsB() == bond.getNumAddBondsB();
        }
        if (bond.getElementB().getSymbol().equals(getElementA().getSymbol()) && 
                bond.getElementA().getSymbol().equals(getElementB().getSymbol())) {
            return bond.getBondType() == bondType &&
                getNumAddBondsB() == bond.getNumAddBondsA() && getNumAddBondsA() == bond.getNumAddBondsB();
        }
        return false;
    }
    
    @Override
    public String toString() {
        if (elementA == null || elementB == null) {
            return "";
        }
        if (elementA.getAtomicNumber() <= elementB.getAtomicNumber()) {
            return "{" + numAddBondsA + "}" + "[" + elementA.getSymbol() + "]" + "{" + bondType.getValue() + "}" + "[" + elementB.getSymbol() + "]" + "{" + numAddBondsB + "}";
        } 
        return "{" + numAddBondsB + "}" + "[" + elementB.getSymbol() + "]" + "{" + bondType.getValue() + "}" + "[" + elementA.getSymbol() + "]" + "{" + numAddBondsA + "}";
    }
}
