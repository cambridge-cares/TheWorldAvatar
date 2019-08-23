/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator;

import org.cam.ceb.como.enthalpy.calculator.solver.SolverHelper;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.species.BondType;
import org.cam.ceb.como.enthalpy.calculator.species.Bond;
import org.cam.ceb.como.tools.periodictable.PeriodicTable;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class SpeciesTest {

    @Test
    public void electronPairTest() {
        System.out.println("C2H4: " + SolverHelper.getNumberOfPairedElectrons(MockSpecies.getC2H4()));
        System.out.println("CH4O: " + SolverHelper.getNumberOfPairedElectrons(MockSpecies.getCH4O()));
        System.out.println("CH3 + OH: " + (SolverHelper.getNumberOfPairedElectrons(MockSpeciesConnectivity.getCH3()) + SolverHelper.getNumberOfPairedElectrons(MockSpeciesConnectivity.getOH())));
        System.out.println("CH4: " + SolverHelper.getNumberOfPairedElectrons(MockSpecies.getCH4()));
        System.out.println("C2H4O: " + SolverHelper.getNumberOfPairedElectrons(MockSpecies.getC2H4O()));
//        
        System.out.println("CH3 + OH: " + (SolverHelper.getNumberOfUnpairedElectrons(MockSpeciesConnectivity.getCH3()) + SolverHelper.getNumberOfUnpairedElectrons(MockSpeciesConnectivity.getOH())));
        System.out.println("C2H4: " + SolverHelper.getNumberOfUnpairedElectrons(MockSpecies.getC2H4()));
        System.out.println("CH4O: " + SolverHelper.getNumberOfUnpairedElectrons(MockSpecies.getCH4O()));
        System.out.println("CH4: " + SolverHelper.getNumberOfUnpairedElectrons(MockSpecies.getCH4()));
        System.out.println("C2H4O: " + SolverHelper.getNumberOfUnpairedElectrons(MockSpecies.getC2H4O()));
        
        System.out.println("O: " + SolverHelper.getNumberOfUnpairedElectrons(MockSpeciesConnectivity.getO()));
        System.out.println("O2: " + SolverHelper.getNumberOfUnpairedElectrons(MockSpeciesConnectivity.getO2()));        
        System.out.println("H2O: " + SolverHelper.getNumberOfUnpairedElectrons(MockSpeciesConnectivity.getH2O()));
    }
    
    @Test
    public void addAtomTest() {
        Species s = new Species("addAtomsTest");

        s.addAtom("C");
        s.addAtom("C");
        s.addAtom("C");
        assert (s.getAtomMap().size() == 3);
        assert (s.getAtomMap().containsKey("C1"));
        assert (s.getAtomMap().containsKey("C2"));
        assert (s.getAtomMap().containsKey("C3"));
        assert (s.getAtomMultiset().count(PeriodicTable.getElementBySymbol("C")) == 3);

        s.addAtom("H");
        s.addAtom("H");
        s.addAtom("H");
        assert (s.getAtomMap().size() == 6);
        assert (s.getAtomMap().containsKey("H1"));
        assert (s.getAtomMap().containsKey("H2"));
        assert (s.getAtomMap().containsKey("H3"));
        assert (s.getAtomMultiset().count(PeriodicTable.getElementBySymbol("H")) == 3);

        s.addAtom("testRef", "Al");
        assert (s.getAtomMap().size() == 7);
        assert (s.getAtomMap().containsKey("testRef"));
        assert (!s.getAtomMap().containsKey("Al"));
        assert (!s.getAtomMap().containsKey("Al"));
        assert (s.getAtomMultiset().count(PeriodicTable.getElementBySymbol("Al")) == 1);

        assert (s.getAtomMultiset().count(PeriodicTable.getElementBySymbol("A")) == 0);
    }

    @Test
    public void addBondTest() {
        Species s = new Species("addBondsTest");

        String c1 = s.addAtom("C");
        String c2 = s.addAtom("C");
        String c3 = s.addAtom("C");
        String h1 = s.addAtom("H");
        String h2 = s.addAtom("H");
        String h3 = s.addAtom("H");

        Bond b1 = new Bond(BondType.SINGLE, c1, c2);
        Bond b2 = new Bond(BondType.DOUBLE, c1, c3);
        Bond b3 = new Bond(BondType.SINGLE, c1, h1);
        Bond b4 = new Bond(BondType.SINGLE, c1, h2);
        Bond b5 = new Bond(BondType.SINGLE, c2, h3);

        s.addBond(b1);
        s.addBond(b2);
        s.addBond(b3);
        s.addBond(b4);
        s.addBond(b5);

        assert (s.getBondMap().size() == 5);
        assert (s.getBondMap().contains(b1));
        assert (s.getBondMap().contains(b2));
        assert (s.getBondMap().contains(b3));
        assert (s.getBondMap().contains(b4));
        assert (s.getBondMap().contains(b5));

        assert (s.getBondTypeMultiset().count("[C]{1}[C]") == 1);
        assert (s.getBondTypeMultiset().count("[C]{2}[C]") == 1);
        assert (s.getBondTypeMultiset().count("[C]{1}[H]") == 3);
        assert (s.getBondTypeMultiset().count("[H]{1}[C]") == 0);
    }

    public void equalsTest() {
        assert (MockSpecies.getC2H4().equals(MockSpecies.getC2H4()));
        assert (MockSpecies.getC2H4O().equals(MockSpecies.getC2H4O()));
        assert (MockSpecies.getC2H6().equals(MockSpecies.getC2H6()));
        assert (MockSpecies.getC2H6O().equals(MockSpecies.getC2H6O()));
        assert (MockSpecies.getC3H6().equals(MockSpecies.getC3H6()));
        assert (MockSpecies.getC3H8().equals(MockSpecies.getC3H8()));
        assert (MockSpecies.getCH4().equals(MockSpecies.getCH4()));
        assert (MockSpecies.getCH4O().equals(MockSpecies.getCH4O()));

        assert (MockSpecies.getC2H4().equals(MockSpecies.getC2H4(), false));
        assert (MockSpecies.getC2H4O().equals(MockSpecies.getC2H4O(), false));
        assert (MockSpecies.getC2H6().equals(MockSpecies.getC2H6(), false));
        assert (MockSpecies.getC2H6O().equals(MockSpecies.getC2H6O(), false));
        assert (MockSpecies.getC3H6().equals(MockSpecies.getC3H6(), false));
        assert (MockSpecies.getC3H8().equals(MockSpecies.getC3H8(), false));
        assert (MockSpecies.getCH4().equals(MockSpecies.getCH4(), false));
        assert (MockSpecies.getCH4O().equals(MockSpecies.getCH4O(), false));

        assert (MockSpecies.getC2H4().equals(MockSpecies.getC2H4(), true));
        assert (MockSpecies.getC2H4O().equals(MockSpecies.getC2H4O(), true));
        assert (MockSpecies.getC2H6().equals(MockSpecies.getC2H6(), true));
        assert (MockSpecies.getC2H6O().equals(MockSpecies.getC2H6O(), true));
        assert (MockSpecies.getC3H6().equals(MockSpecies.getC3H6(), true));
        assert (MockSpecies.getC3H8().equals(MockSpecies.getC3H8(), true));
        assert (MockSpecies.getCH4().equals(MockSpecies.getCH4(), true));
        assert (MockSpecies.getCH4O().equals(MockSpecies.getCH4O(), true));
        
        Species C2H4 = MockSpecies.getC2H4();
        C2H4.setTotalEnergy(C2H4.getTotalEnergy() * 0.05 + C2H4.getTotalEnergy());
        Species C2H4O = MockSpecies.getC2H4O();
        C2H4O.setTotalEnergy(C2H4O.getTotalEnergy() * 0.05 + C2H4O.getTotalEnergy());
        Species C2H6 = MockSpecies.getC2H6();
        C2H6.setTotalEnergy(C2H6.getTotalEnergy() * 0.05 + C2H6.getTotalEnergy());
        Species C2H6O = MockSpecies.getC2H6O();
        C2H6O.setTotalEnergy(C2H6O.getTotalEnergy() * 0.05 + C2H6O.getTotalEnergy());
        Species C3H6 = MockSpecies.getC3H6();
        C3H6.setTotalEnergy(C3H6.getTotalEnergy() * 0.05 + C3H6.getTotalEnergy());
        Species C3H8 = MockSpecies.getC3H8();
        C3H8.setTotalEnergy(C3H8.getTotalEnergy() * 0.05 + C3H8.getTotalEnergy());
        Species CH4 = MockSpecies.getCH4();
        CH4.setTotalEnergy(CH4.getTotalEnergy() * 0.05 + CH4.getTotalEnergy());
        Species CH4O = MockSpecies.getCH4O();
        CH4O.setTotalEnergy(CH4O.getTotalEnergy() * 0.05 + CH4O.getTotalEnergy());
        
        assert (MockSpecies.getC2H4().equals(C2H4, true));
        assert (MockSpecies.getC2H4O().equals(C2H4O, true));
        assert (MockSpecies.getC2H6().equals(C2H6, true));
        assert (MockSpecies.getC2H6O().equals(C2H6O, true));
        assert (MockSpecies.getC3H6().equals(C3H6, true));
        assert (MockSpecies.getC3H8().equals(C3H8, true));
        assert (MockSpecies.getCH4().equals(CH4, true));
        assert (MockSpecies.getCH4O().equals(CH4O, true));
        
        C2H4 = MockSpecies.getC2H4();
        C2H4.setTotalEnergy(C2H4.getTotalEnergy() * 0.1 + C2H4.getTotalEnergy());
        C2H4O = MockSpecies.getC2H4O();
        C2H4O.setTotalEnergy(C2H4O.getTotalEnergy() * 0.25 + C2H4O.getTotalEnergy());
        C2H6 = MockSpecies.getC2H6();
        C2H6.setTotalEnergy(C2H6.getTotalEnergy() * 0.11 + C2H6.getTotalEnergy());
        C2H6O = MockSpecies.getC2H6O();
        C2H6O.setTotalEnergy(C2H6O.getTotalEnergy() * 0.100001 + C2H6O.getTotalEnergy());
        C3H6 = MockSpecies.getC3H6();
        C3H6.setTotalEnergy(C3H6.getTotalEnergy() * 0.15 + C3H6.getTotalEnergy());
        C3H8 = MockSpecies.getC3H8();
        C3H8.setTotalEnergy(C3H8.getTotalEnergy() * 0.8 + C3H8.getTotalEnergy());
        CH4 = MockSpecies.getCH4();
        CH4.setTotalEnergy(CH4.getTotalEnergy() * 0.75 + CH4.getTotalEnergy());
        CH4O = MockSpecies.getCH4O();
        CH4O.setTotalEnergy(CH4O.getTotalEnergy() * 0.15 + CH4O.getTotalEnergy());
        
        assert (MockSpecies.getC2H4().equals(C2H4, true));
        assert (!MockSpecies.getC2H4O().equals(C2H4O, true));
        assert (!MockSpecies.getC2H6().equals(C2H6, true));
        assert (!MockSpecies.getC2H6O().equals(C2H6O, true));
        assert (!MockSpecies.getC3H6().equals(C3H6, true));
        assert (!MockSpecies.getC3H8().equals(C3H8, true));
        assert (!MockSpecies.getCH4().equals(CH4, true));
        assert (!MockSpecies.getCH4O().equals(CH4O, true));
    }
}
