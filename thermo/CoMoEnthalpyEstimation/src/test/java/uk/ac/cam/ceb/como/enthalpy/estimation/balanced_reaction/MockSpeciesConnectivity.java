/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.BondType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;

/**
 *
 * @author pb556
 */
public class MockSpeciesConnectivity {

    public static Species getC8H10() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 8);
        ms.add(PeriodicTable.getElementBySymbol("H"), 10);

        Species species = getSpecies("C8H10", ms);

        // create connectivity
        species.addBond(BondType.DOUBLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C2", "C3");
        species.addBond(BondType.SINGLE, "C2", "C4");
        species.addBond(BondType.SINGLE, "C4", "C5");
        species.addBond(BondType.DOUBLE, "C4", "C6");
        species.addBond(BondType.SINGLE, "C6", "C7");
        species.addBond(BondType.TRIPLE, "C7", "C8");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C3", "H3");
        species.addBond(BondType.SINGLE, "C3", "H4");
        species.addBond(BondType.SINGLE, "C3", "H5");
        species.addBond(BondType.SINGLE, "C5", "H6");
        species.addBond(BondType.SINGLE, "C5", "H7");
        species.addBond(BondType.SINGLE, "C5", "H8");
        species.addBond(BondType.SINGLE, "C6", "H9");
        species.addBond(BondType.SINGLE, "C8", "H10");

        return species;
    }

    public static Species getC2H2() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 2);
        ms.add(PeriodicTable.getElementBySymbol("H"), 2);

        Species species = getSpecies("C2H2", ms);

        // create connectivity
        species.addBond(BondType.TRIPLE, "C1", "C2");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C2", "H2");

        return species;
    }
    
    public static Species getC4H8() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 4);
        ms.add(PeriodicTable.getElementBySymbol("H"), 8);

        Species species = getSpecies("C4H8", ms);

        // create connectivity
        species.addBond(BondType.SINGLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C3", "C4");
        species.addBond(BondType.DOUBLE, "C2", "C3");

        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C1", "H3");
        species.addBond(BondType.SINGLE, "C2", "H4");
        species.addBond(BondType.SINGLE, "C3", "H5");
        species.addBond(BondType.SINGLE, "C4", "H6");
        species.addBond(BondType.SINGLE, "C4", "H7");
        species.addBond(BondType.SINGLE, "C4", "H8");

        return species;
    }

    public static Species getIsoC4H8() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 4);
        ms.add(PeriodicTable.getElementBySymbol("H"), 8);

        Species species = getSpecies("C4H8", ms);

        // create connectivity
        species.addBond(BondType.SINGLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C1", "C3");
        species.addBond(BondType.DOUBLE, "C1", "C4");

        species.addBond(BondType.SINGLE, "C2", "H1");
        species.addBond(BondType.SINGLE, "C2", "H2");
        species.addBond(BondType.SINGLE, "C2", "H3");
        species.addBond(BondType.SINGLE, "C3", "H4");
        species.addBond(BondType.SINGLE, "C3", "H5");
        species.addBond(BondType.SINGLE, "C3", "H6");
        species.addBond(BondType.SINGLE, "C4", "H7");
        species.addBond(BondType.SINGLE, "C4", "H8");

        return species;
    }

    public static Species getC3H4() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 3);
        ms.add(PeriodicTable.getElementBySymbol("H"), 4);

        Species species = getSpecies("C3H4", ms);

        // create connectivity
        species.addBond(BondType.TRIPLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C2", "C3");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C3", "H2");
        species.addBond(BondType.SINGLE, "C3", "H3");
        species.addBond(BondType.SINGLE, "C3", "H4");

        return species;
    }

    public static Species getC6H10() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 6);
        ms.add(PeriodicTable.getElementBySymbol("H"), 10);

        Species species = getSpecies("C6H10", ms);

        // create connectivity
        species.addBond(BondType.DOUBLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C2", "C3");
        species.addBond(BondType.SINGLE, "C2", "C4");
        species.addBond(BondType.SINGLE, "C4", "C5");
        species.addBond(BondType.DOUBLE, "C4", "C6");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C3", "H3");
        species.addBond(BondType.SINGLE, "C3", "H4");
        species.addBond(BondType.SINGLE, "C3", "H5");
        species.addBond(BondType.SINGLE, "C5", "H6");
        species.addBond(BondType.SINGLE, "C5", "H7");
        species.addBond(BondType.SINGLE, "C5", "H8");
        species.addBond(BondType.SINGLE, "C6", "H9");
        species.addBond(BondType.SINGLE, "C6", "H10");

        return species;
    }

    public static Species getC4H4() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 4);
        ms.add(PeriodicTable.getElementBySymbol("H"), 4);

        Species species = getSpecies("C4H4", ms);

        // create connectivity
        species.addBond(BondType.TRIPLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C2", "C3");
        species.addBond(BondType.DOUBLE, "C3", "C4");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C3", "H2");
        species.addBond(BondType.SINGLE, "C4", "H3");
        species.addBond(BondType.SINGLE, "C4", "H4");

        return species;
    }

    public static Species getC5H8() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 5);
        ms.add(PeriodicTable.getElementBySymbol("H"), 8);

        Species species = getSpecies("C5H8", ms);

        // create connectivity
        species.addBond(BondType.DOUBLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C3", "C2");
        species.addBond(BondType.SINGLE, "C2", "C4");
        species.addBond(BondType.SINGLE, "C4", "C5");
        
        species.addBond(BondType.SINGLE, "C1", "H1");
        species.addBond(BondType.SINGLE, "C1", "H2");
        species.addBond(BondType.SINGLE, "C3", "H3");
        species.addBond(BondType.SINGLE, "C3", "H4");
        species.addBond(BondType.SINGLE, "C3", "H5");
        species.addBond(BondType.SINGLE, "C4", "H6");
        species.addBond(BondType.SINGLE, "C5", "H7");
        species.addBond(BondType.SINGLE, "C5", "H8");

        return species;
    }
    
    public static Species getC5H10() {

        Multiset<Element> ms = HashMultiset.create();
        ms.add(PeriodicTable.getElementBySymbol("C"), 5);
        ms.add(PeriodicTable.getElementBySymbol("H"), 10);

        Species species = getSpecies("C5H10", ms);

        // create connectivity
        species.addBond(BondType.SINGLE, "C1", "C2");
        species.addBond(BondType.SINGLE, "C1", "C3");
        species.addBond(BondType.DOUBLE, "C1", "C4");
        species.addBond(BondType.SINGLE, "C4", "C5");
        
        species.addBond(BondType.SINGLE, "C2", "H1");
        species.addBond(BondType.SINGLE, "C2", "H2");
        species.addBond(BondType.SINGLE, "C2", "H3");
        species.addBond(BondType.SINGLE, "C3", "H4");
        species.addBond(BondType.SINGLE, "C3", "H5");
        species.addBond(BondType.SINGLE, "C3", "H6");
        species.addBond(BondType.SINGLE, "C4", "H7");
        species.addBond(BondType.SINGLE, "C5", "H8");
        species.addBond(BondType.SINGLE, "C5", "H9");
        species.addBond(BondType.SINGLE, "C5", "H10");

        return species;
    }

    public static Species getCH3() {

        Species species = new Species("CH3", 0, 0);
        species.addAtom("C", PeriodicTable.getElementBySymbol("C"));
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H3", PeriodicTable.getElementBySymbol("H"));

        species.addBond(BondType.SINGLE, "C", "H1");
        species.addBond(BondType.SINGLE, "C", "H2");
        species.addBond(BondType.SINGLE, "C", "H3");

        return species;

    }

    public static Species getOH() {

        Species species = new Species("OH", 0, 0);
        species.addAtom("H", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("O", PeriodicTable.getElementBySymbol("O"));

        species.addBond(BondType.SINGLE, "O", "H");

        return species;

    }

    public static Species getH2O() {
        Species species = new Species("H2O", 0, 0);
        species.addAtom("H1", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("H2", PeriodicTable.getElementBySymbol("H"));
        species.addAtom("O", PeriodicTable.getElementBySymbol("O"));

        species.addBond(BondType.SINGLE, "O", "H1");
        species.addBond(BondType.SINGLE, "O", "H2");

        return species;
    }

    public static Species getO() {
        Species species = new Species("O", 0, 0);
        species.addAtom("O1", PeriodicTable.getElementBySymbol("O"));

        return species;
    }

    public static Species getO2() {
        Species species = new Species("O2", 0, 0);
        species.addAtom("O1", PeriodicTable.getElementBySymbol("O"));
        species.addAtom("O2", PeriodicTable.getElementBySymbol("O"));

        species.addBond(BondType.DOUBLE, "O1", "O2");

        return species;
    }

    public static Species getH() {
        Species species = new Species("H", 0, 0);
        species.addAtom("H", PeriodicTable.getElementBySymbol("H"));

        return species;
    }

    public static Species getCH4() {
        Species species = MockSpecies.getCH4();
        species.setHf(0.0);
        species.setTotalEnergy(0.0);
        return species;
    }
    
    public static Species getC2H4() {
        Species species = MockSpecies.getC2H4();
        species.setHf(0.0);
        species.setTotalEnergy(0.0);
        return species;
    }
    
    public static Species getC2H6() {
        Species species = MockSpecies.getC2H6();
        species.setHf(0.0);
        species.setTotalEnergy(0.0);
        return species;
    }
    
    public static Species getC3H6() {
        Species species = MockSpecies.getC3H6();
        species.setHf(0.0);
        species.setTotalEnergy(0.0);
        return species;
    }
    
    protected static Species getSpecies(String ref, Multiset<Element> ms) {
        Species species = new Species(ref, 0, 0);
        for (Element e : ms.elementSet()) {
            for (int i = 1; i <= ms.count(e); i++) {
                species.addAtom(e.getSymbol() + i, e);
            }
        }
        return species;
    }
}