/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.reaction.selector;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.species.Bond;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.tools.periodictable.Element;
import org.cam.ceb.como.tools.periodictable.PeriodicTable;

/**
 *
 * @author pb556
 */
public class AtomBondTypeSelector extends ReactionSelector {

    protected int val = 0;
    protected boolean isErrThreshold = false;

    public AtomBondTypeSelector(int val, boolean isErrThreshold) {
        this.val = val;
        this.isErrThreshold = isErrThreshold;
    }

    // use only symbols and no proper references for the bonds objects
    @Override
    public ReactionList select(ReactionList reactions) {
        ReactionList rList = new ReactionList();
        HashMap<Reaction, Integer> data = new HashMap<Reaction, Integer>();
        for (Reaction r : reactions) {
            data.put(r, getNumberOfErrors(r));
            if (isErrThreshold) {
                if (getNumberOfErrors(r) <= val) {
                    rList.add(r);
                }
            }
        }
        if (!isErrThreshold) {
            int err = 0;
            while (true) {
                for (Reaction r : data.keySet()) {
                    if (data.get(r) == err) {
                        rList.add(r);
                    }
                }
                err++;
                if (rList.size() >= val) {
                    break;
                }
            }
        }
        return rList;
    }

    // does not work properly!!!
    public int getNumberOfErrors(Reaction r) {
        // comparison between both sides
        HashMap<AtomBondType, Integer> reactantsDataComplete = combine(r.getReactants().keySet());
        HashMap<AtomBondType, Integer> productsDataComplete = combine(r.getProducts().keySet());

        // compare reactants and products side
        int num = 0;

        for (AtomBondType a1 : reactantsDataComplete.keySet()) {
            int a1Num = reactantsDataComplete.get(a1);
            boolean identified = false;
            for (AtomBondType a2 : productsDataComplete.keySet()) {
                int a2Num = productsDataComplete.get(a2);
                if (a1.equals(a2)) {
                    num += Math.abs(a1Num - a2Num);
                    productsDataComplete.remove(a2);
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                num += a1Num;
            }
        }

        for (AtomBondType a2 : productsDataComplete.keySet()) {
            num += productsDataComplete.get(a2);
        }

        return num;
    }

    private HashMap<AtomBondType, Integer> combine(Collection<Species> species) {
        HashMap<AtomBondType, Integer> combined = new HashMap<AtomBondType, Integer>();
        for (Species s : species) {
            HashMap<AtomBondType, Integer> data = getAtomBondTypes(s);
            int num = 0;
            for (AtomBondType a : data.keySet()) {
                num = data.get(a);
                boolean identified = false;
                for (AtomBondType a2 : combined.keySet()) {
                    if (a.equals(a2)) {
                        identified = true;
                        num += combined.get(a2);
                        combined.put(a2, num);
                        break;
                    }
                }
                if (!identified) {
                    combined.put(a, num);
                }
            }
        }
        return combined;
    }

    private HashMap<AtomBondType, Integer> getAtomBondTypes(Species s) {
        // go through each atom
        Map<String, String> atoms = s.getAtomMap();
        Collection<Bond> bonds = s.getBondMap();

        HashMap<AtomBondType, Integer> atomBondTypes = new HashMap<AtomBondType, Integer>();

        for (String refAtom : atoms.keySet()) {
            ArrayList<Bond> atomBond = new ArrayList<Bond>();
            for (Bond b : bonds) {
                if (b.getRefAtomA().equals(refAtom) || b.getRefAtomB().equals(refAtom)) {
                    atomBond.add(b);
                }
            }
            if (atomBond.size() > 1) {
                AtomBondType abt = new AtomBondType(PeriodicTable.getElementBySymbol(atoms.get(refAtom)));
                for (Bond b : atomBond) {
                    String atomA = "";
                    String atomB = "";
                    for (String ref : atoms.keySet()) {
                        if (b.getRefAtomA().equals(ref)) {
                            atomA = atoms.get(ref);
                        }
                        if (b.getRefAtomB().equals(ref)) {
                            atomB = atoms.get(ref);
                        }
                    }
                    if (atomA.length() != 0 && atomB.length() != 0) {
                        abt.addBond(new Bond(b.getBondType(), atomA, atomB), 1);
                    }
                }
                int num = 1;
                for (AtomBondType a : atomBondTypes.keySet()) {
                    if (a.equals(abt)) {
                        num += atomBondTypes.get(a);
                        atomBondTypes.remove(a);
                        break;
                    }
                }
                atomBondTypes.put(abt, num);
            }
        }

        return atomBondTypes;
    }

    private class AtomBondType {

        protected Element e = null;
        protected HashMap<Bond, Integer> bonds = new HashMap<Bond, Integer>();

        public AtomBondType(Element e) {
            this.e = e;
        }

        public void addBond(Bond b, int num) {
            for (Bond bRef : bonds.keySet()) {
                if (b.equals(bRef)) {
                    num += bonds.get(bRef);
                    bonds.remove(bRef);
                    break;
                }
            }
            bonds.put(b, num);
        }

        public boolean equals(AtomBondType cmp) {
            boolean equal = true;
            equal &= e.getSymbol().equalsIgnoreCase(cmp.e.getSymbol());
            for (Bond b1 : bonds.keySet()) {
                boolean identified = false;
                for (Bond b2 : cmp.bonds.keySet()) {
                    if (b1.equals(b2) && bonds.get(b1) == cmp.bonds.get(b2)) {
                        identified = true;
                        break;
                    }
                }
                if (!identified) {
                    return false;
                }
            }
            return equal;
        }
    }
}
