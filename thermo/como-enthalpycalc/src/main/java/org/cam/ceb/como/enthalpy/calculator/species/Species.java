/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.species;

import com.google.common.collect.HashMultiset;
import org.cam.ceb.como.tools.periodictable.Element;
import com.google.common.collect.Multiset;
import com.google.common.collect.Ordering;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import org.apache.log4j.Logger;
import org.cam.ceb.como.tools.periodictable.PeriodicTable;

/**
 *
 * @author pb556
 */
public class Species {

    private String ref;
    private double hf;
    private double totalEnergy;
    private final HashMap<String, Element> atoms = new HashMap<String, Element>();
    private final Collection<Bond> bonds = new HashSet<Bond>();
    private final Multiset<String> msBondTypes = HashMultiset.create();
    private final Multiset<Element> msAtoms = HashMultiset.create();
    private Logger logger = Logger.getLogger(Species.class);

    /**
     * Create an ISDSpecies with a supplied ref. The enthalpy of formation and
     * the total energy need to be set separately. Please noted that the
     * temperature used for every ISDSpecies must be the same. This isodesmic
     * library does not correct the value of energy because the correction
     * depends on the method used. For example, if an scaling factor is used,
     * then the value of dHf is different.
     *
     * @param ref reference of <code>T</code> type for looking up.
     */
    public Species(String ref) {
        this.ref = ref;
    }

    /**
     * Create an ISDSpecies with a supplied ref, enthalpy of formation and total
     * energy. Please noted that the temperature used for every ISDSpecies must
     * be the same. This isodesmic library does not correct the value of energy
     * because the correction depends on the method used. For example, if an
     * scaling factor is used, then the value of dHf is different.
     *
     * @param ref reference of <code>T</code> type for looking up.
     * @param hf enthalpy of formation at T K in J/mol
     * @param totalEnergy total energy (electronic energy + zero point energy [+
     * dHf if T is not zero])
     */
    public Species(String ref, double hf, double totalEnergy) {
        this.ref = ref;
        this.hf = hf;
        this.totalEnergy = totalEnergy;
    }
    
    public void setRef(String ref) {
        this.ref = ref;
    }

    /**
     * Get the value of reference.
     *
     * @return value of reference
     */
    public String getRef() {
        return ref;
    }

    /**
     * Get enthalpy of formation in J/mol
     *
     * @return enthalpy of formation in J/mol
     */
    public double getHf() {
        return hf;
    }

    /**
     * Set enthalpy of formation in J/mol
     *
     * @param hf enthalpy of formation in J/mol
     */
    public void setHf(double hf) {
        this.hf = hf;
    }

    /**
     * Get total energy in J/mol
     *
     * @return total energy in J/mol
     */
    public double getTotalEnergy() {
        return totalEnergy;
    }

    /**
     * Set total energy in J/mol
     *
     * @param totalEnergy total energy in J/mol
     */
    public void setTotalEnergy(double totalEnergy) {
        this.totalEnergy = totalEnergy;
    }

    /**
     * Get a multiset of atoms. This is useful for counting the number of each
     * atom type (element).
     *
     * @return a multiset of atoms
     */
    public Multiset<Element> getAtomMultiset() {
        return msAtoms;
    }

    /**
     * Get a multiset of bond types. This is useful for counting the number of
     * each bond type.
     *
     * @return a multiset of bond types
     */
    public Multiset<String> getBondTypeMultiset() {
        return msBondTypes;
    }

    public Map<String, String> getAtomMap() {
        HashMap<String, String> map = new HashMap<String, String>();
        for (String ref : atoms.keySet()) {
            map.put(ref, atoms.get(ref).getSymbol());
        }
        return map;
    }

    public Collection<Bond> getBondMap() {
        return bonds;
    }

    /**
     * Add an atom of a specified element symbol
     *
     * @param element element symbol
     */
    public String addAtom(String element) {
        String ref = getRef(element);
        addAtom(ref, PeriodicTable.getElementBySymbol(element.trim()));
        return ref;
    }

    public void addAtom(String ref, Element element) {
        if (element == null) {
            logger.error("No element is defined!");
            return;
        }
        if (atoms.get(ref) != null) {
            logger.warn("An atom with the reference " + ref + " is already included!");
            return;
        }
        atoms.put(ref, element);
        msAtoms.add(element);
    }

    public void addAtom(String ref, String elementSymbol) {
        addAtom(ref, PeriodicTable.getElementBySymbol(elementSymbol));
    }

    /**
     * Add a specific number,
     * <code>count</code>, of atoms of
     * <code>element</code>
     *
     * @param element element symbol
     * @param count number of atoms
     */
    //public abstract void addAtom(String element, int count);
    /**
     * Add a bond of specified bond type and elements. Switching between atomA
     * and atomB in the arguments makes no difference in the calculation. This
     * method is equivalent to using
     * <code>addBond(bondType, atomA, atomB, 1)</code>
     *
     * @param bondType bond type
     * @param atomA element symbol of the atom on one end
     * @param atomB element symbol of the atom on the other end
     */
    public void addBond(BondType bondType, String atomA, String atomB) {
        addBond(new Bond<String>(bondType, atomA, atomB));
    }

    public void addBond(Bond bond) {
        bonds.add(bond);
        msBondTypes.add(toBondString(bond));
    }

    /**
     * Get a multiset of atoms. This is useful for counting the number of each
     * atom type (element).
     *
     * @return a multiset of atoms
     */
    public Map<String, Element> getElementMap() {
        return atoms;
    }

    /**
     * Add a specific number,
     * <code>count</code>, of bonds of specified bond type and elements.
     * Switching between atomA and atomB in the arguments makes no difference in
     * the calculation.
     *
     * @param bondType bond type
     * @param atomA element symbol of the atom on one end
     * @param atomB element symbol of the atom on the other end
     * @param count number of bonds
     */
    //public abstract void addBond(BondType bondType, String atomA, String atomB, int count);
    //public abstract void addBond(Bond bond, int count);
    public boolean equals(Species s, boolean spOfInterest) {
        if (spOfInterest) {
            if (getTotalEnergy() != s.getTotalEnergy()) {
                return false;
            }
        } else {
            if (getTotalEnergy() != s.getTotalEnergy()
                    || getHf() != s.getHf()
                    || getRef().compareToIgnoreCase(s.getRef()) != 0) {
                return false;
            }
        }
        for (Object element1 : getAtomMultiset().elementSet()) {
            String strElement1 = element1.toString();
            int count1 = getAtomMultiset().count(element1);
            boolean identified = false;
            for (Object element2 : s.getAtomMultiset().elementSet()) {
                if (strElement1.equals(element2.toString())) {
                    if (count1 == s.getAtomMultiset().count(element2)) {
                        identified = true;
                        break;
                    }
                }
            }
            if (!identified) {
                return false;
            }
        }
        for (Object element1 : getBondTypeMultiset().elementSet()) {
            String strElement1 = element1.toString();
            int count1 = getBondTypeMultiset().count(element1);
            boolean identified = false;
            for (Object element2 : s.getBondTypeMultiset().elementSet()) {
                if (strElement1.equals(element2.toString())) {
                    if (count1 == s.getBondTypeMultiset().count(element2)) {
                        identified = true;
                        break;
                    }
                }
            }
            if (!identified) {
                return false;
            }
        }
        return true;
    }

    public String toBondString(Bond b) {
        final String representationTemplate = "[%s]{%s}[%s]";

//        Collator collator = Collator.getInstance(Locale.UK);
//        collator.setStrength(Collator.SECONDARY);
//        //int comparison = collator.compare(b.getRefAtomA().toString(), b.getRefAtomB().toString());
//        int comparison = b.getRefAtomA().toString().compareTo(b.getRefAtomB().toString());
//        
//        ArrayList<String> list = new ArrayList<String>();
//        list.add(b.getRefAtomA().toString());
//        list.add(b.getRefAtomB().toString());
//        System.out.println (Ordering.natural().isOrdered(list));
//        
        if (getAtomMap().get(b.getRefAtomA().toString()).compareTo(getAtomMap().get(b.getRefAtomB().toString())) < 0) {
            //System.out.println(String.format(representationTemplate, getAtomMap().get(b.getRefAtomA().toString()), b.getBondType().getValue(), getAtomMap().get(b.getRefAtomB().toString())));
            return String.format(representationTemplate, getAtomMap().get(b.getRefAtomA().toString()), b.getBondType().getValue(), getAtomMap().get(b.getRefAtomB().toString()));
        } else {
            //System.out.println(String.format(representationTemplate, getAtomMap().get(b.getRefAtomB().toString()), b.getBondType().getValue(), getAtomMap().get(b.getRefAtomA().toString())));
            return String.format(representationTemplate, getAtomMap().get(b.getRefAtomB().toString()), b.getBondType().getValue(), getAtomMap().get(b.getRefAtomA().toString()));
        }
    }

    public Species clone() {
        Species s = new Species(this.getRef(), this.getHf(), this.getTotalEnergy());
        for (String ref : this.atoms.keySet()) {
            s.addAtom(ref, this.atoms.get(ref));
        }
        for (Bond b : this.bonds) {
            s.addBond(b.clone());
        }
        return s;
    }

    protected String getRef(String element) {
        int ctr = 1;
        boolean identified = false;
        for (String ref : atoms.keySet()) {
            if (ref.equals(element + ctr)) {
                identified = true;
                break;
            }
        }
        if (identified) {
            while (identified) {
                identified = false;
                for (String ref : atoms.keySet()) {
                    if (ref.equals(element + ctr)) {
                        identified = true;
                        ctr++;
                        break;
                    }
                }
            }
        }
        return element + ctr;
    }
}
