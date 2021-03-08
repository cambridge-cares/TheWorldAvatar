/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.species.homodesmotic;

import org.cam.ceb.como.enthalpy.calculator.species.BondType;
import org.cam.ceb.como.tools.periodictable.Element;

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
