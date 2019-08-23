/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.species.homodesmotic;

import com.google.common.collect.Multiset;
import java.util.ArrayList;
import java.util.Collections;
import org.cam.ceb.como.tools.periodictable.Element;

/**
 *
 * @author pb556
 */
public class HypohomodesmoticAtom {

    protected Multiset<HypohomodesmoticBond> bonds;
    protected Element element;

    public HypohomodesmoticAtom(Element element, Multiset<HypohomodesmoticBond> bonds) {
        this.bonds = bonds;
        this.element = element;
    }

    public Element getElement() {
        return element;
    }

    public Multiset<HypohomodesmoticBond> getBonds() {
        return bonds;
    }

    public boolean equals(HypohomodesmoticAtom atom) {
        if (!atom.getElement().getSymbol().equals(element.getSymbol()) || bonds.size() != atom.getBonds().size()) {
            return false;
        }
        // iteration through the bond types
        for (HypohomodesmoticBond bt : bonds) {
            HypohomodesmoticBond cmp = null;
            for (HypohomodesmoticBond btAtom : atom.getBonds()) {
                if (bt.equals(btAtom)) {
                    cmp = btAtom;
                    break;
                }
            }
            if (cmp == null) {
                return false;
            }
            if (atom.getBonds().count(cmp) != bonds.count(bt)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        String retVal = "[" + element.getSymbol() + "]";
        ArrayList<String> strBonds = getSortedBondsString();
        for (String b : strBonds) {
            retVal += b;
        }
        return retVal;
    }

    protected ArrayList<String> getSortedBondsString() {
        ArrayList<String> b = new ArrayList<String>();
        for (HypohomodesmoticBond bt : bonds) {
            //for (int i = 0; i < bonds.count(bt); i++) {
            b.add(bt.toString());
            //}
        }
        Collections.sort(b);
        return b;
    }
}