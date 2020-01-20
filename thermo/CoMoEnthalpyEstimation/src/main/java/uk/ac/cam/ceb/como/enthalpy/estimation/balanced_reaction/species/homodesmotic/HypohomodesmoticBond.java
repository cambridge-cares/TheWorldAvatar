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
public class HypohomodesmoticBond {
    
    protected Element element;
    protected BondType bondType;
    
    public HypohomodesmoticBond(Element element, BondType bondType) {
        this.element = element;
        this.bondType = bondType;
    }
    
    public Element getElement() {
        return element;
    }
    
    public BondType getBondType() {
        return bondType;
    }
    
    public boolean equals(HypohomodesmoticBond bond) {
        return element.getSymbol().equals(bond.getElement().getSymbol()) && bondType.equals(bond.getBondType());
    }
    
    @Override
    public String toString() {
        return "([" + element.getSymbol() + "]{" + bondType.getValue() + "})";
    }
}
