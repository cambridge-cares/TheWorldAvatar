/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import org.junit.Test;

import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.periodictable.PeriodicTable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpeciesConnectivity;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 * 
 */
public class ReactionTypeTest {

    // only the non abstract methods are tested here
    @Test
    public void CHAtomsTest() {
        Collection<Element> contained = new HashSet<Element>();
        Collection<Element> notContained = new HashSet<Element>();

        contained.add(PeriodicTable.getElementBySymbol("C"));
        contained.add(PeriodicTable.getElementBySymbol("H"));
        notContained.add(PeriodicTable.getElementBySymbol("O"));
        notContained.add(PeriodicTable.getElementBySymbol("Ti"));

        assert (testMassBalance(MockSpecies.getC2H4(), contained, notContained));
        assert (testMassBalance(MockSpecies.getC2H6(), contained, notContained));
        assert (testMassBalance(MockSpecies.getC3H6(), contained, notContained));
        assert (testMassBalance(MockSpecies.getC3H8(), contained, notContained));
//        assert (testMassBalance(MockSpecies.getC5H11(), contained, notContained));
//        assert (testMassBalance(MockSpecies.getC6H10(), contained, notContained));
//        assert (testMassBalance(MockSpecies.getC6H14(), contained, notContained));
//        assert (testMassBalance(MockSpecies.getC8H16(), contained, notContained));
        assert (testMassBalance(MockSpeciesConnectivity.getCH3(), contained, notContained));
        assert (testMassBalance(MockSpecies.getCH4(), contained, notContained));
    }

    @Test
    public void CHOAtomsTest() {
        Collection<Element> contained = new HashSet<Element>();
        Collection<Element> notContained = new HashSet<Element>();

        contained.add(PeriodicTable.getElementBySymbol("C"));
        contained.add(PeriodicTable.getElementBySymbol("H"));
        contained.add(PeriodicTable.getElementBySymbol("O"));
        notContained.add(PeriodicTable.getElementBySymbol("Ti"));

        assert (testMassBalance(MockSpecies.getC2H4O(), contained, notContained));
        assert (testMassBalance(MockSpecies.getC2H6O(), contained, notContained));
        assert (testMassBalance(MockSpecies.getCH4O(), contained, notContained));
    }

    @Test
    public void OHAtomsTest() {
        Collection<Element> contained = new HashSet<Element>();
        Collection<Element> notContained = new HashSet<Element>();

        notContained.add(PeriodicTable.getElementBySymbol("C"));
        contained.add(PeriodicTable.getElementBySymbol("H"));
        contained.add(PeriodicTable.getElementBySymbol("O"));
        notContained.add(PeriodicTable.getElementBySymbol("Ti"));

        assert (testMassBalance(MockSpeciesConnectivity.getOH(), contained, notContained));
        assert (testMassBalance(MockSpeciesConnectivity.getH2O(), contained, notContained));
    }

    protected boolean testMassBalance(Species species, Collection<Element> contained, Collection<Element> notContained) {
        ReactionType rt = new ISDReactionType();
        boolean valid = true;
        Collection<Element> el = rt.getAllElements(species);
        Collection<String> eSymbols = rt.getAllElementSymbols(species);
        if (contained != null) {
            valid &= contained.size() == el.size();
            for (Element e : contained) {
                valid &= eSymbols.contains(e.getSymbol()) && el.contains(e);
            }
        }
        if (notContained != null) {
            for (Element e : notContained) {
                valid &= !eSymbols.contains(e.getSymbol()) && !el.contains(e);
            }
        }
        return valid;
    }

    protected boolean testMassBalance(Collection<Species> species, Collection<Element> contained, Collection<Element> notContained, HashMap<Variable, List<Integer>> mBalance, VariableSet vSet) {
        ReactionType rt = new ISDReactionType();
        boolean valid = true;
        Collection<Element> el = rt.getAllElements(species);
        Collection<String> eSymbols = rt.getAllElementSymbols(species);
        if (contained != null) {
            valid &= contained.size() == el.size();
            for (Element e : contained) {
                valid &= eSymbols.contains(e.getSymbol()) && el.contains(e);
            }
        }
        if (notContained != null) {
            for (Element e : notContained) {
                valid &= !eSymbols.contains(e.getSymbol()) && !el.contains(e);
            }
        }

        HashMap<Variable, List<Integer>> calcMBalance = rt.getMassBalanceConstraints(species, vSet);
        for (Variable v1 : mBalance.keySet()) {
            valid &= calcMBalance.containsKey(v1);
            if (valid) {
                List<Integer> l1 = calcMBalance.get(v1);
                List<Integer> l2 = calcMBalance.get(v1);
                valid &= l1.size() == l2.size();
                if (valid) {
                    for (int i = 0; i < l1.size(); i++) {
                        valid &= l2.contains(l1.get(i));
                    }
                }
            }
        }

        return valid;
    }

    @Test
    public void poolCHOTest() {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H8 = MockSpecies.getC3H8();

        Collection<Species> species = new HashSet<Species>();
        species.add(C2H4);
        species.add(C2H4O);
        species.add(C2H6);
        species.add(C2H6O);
        species.add(C3H8);

        Collection<Element> contained = new HashSet<Element>();
        Collection<Element> notContained = new HashSet<Element>();

        contained.add(PeriodicTable.getElementBySymbol("C"));
        contained.add(PeriodicTable.getElementBySymbol("H"));
        contained.add(PeriodicTable.getElementBySymbol("O"));
        notContained.add(PeriodicTable.getElementBySymbol("Ti"));

        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        HashMap<Variable, List<Integer>> mBalance = new HashMap<Variable, List<Integer>>();

        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(2);
        l.add(4);
        l.add(0);
        mBalance.put(vSet.getVariableOf(C2H4), l);

        l = new ArrayList<Integer>();
        l.add(2);
        l.add(4);
        l.add(1);
        mBalance.put(vSet.getVariableOf(C2H4O), l);
        
        l = new ArrayList<Integer>();
        l.add(2);
        l.add(6);
        l.add(0);
        mBalance.put(vSet.getVariableOf(C2H6), l);
        
        l = new ArrayList<Integer>();
        l.add(2);
        l.add(6);
        l.add(1);
        mBalance.put(vSet.getVariableOf(C2H6O), l);
        
        l = new ArrayList<Integer>();
        l.add(3);
        l.add(8);
        l.add(0);
        mBalance.put(vSet.getVariableOf(C3H8), l);

        assert(testMassBalance(species, contained, notContained, mBalance, vSet));
    }
    
    @Test
    public void poolCHTest1() {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H6 = MockSpecies.getC2H6();
        Species C3H8 = MockSpecies.getC3H8();

        Collection<Species> species = new HashSet<Species>();
        species.add(C2H4);
        species.add(C2H6);
        species.add(C3H8);

        Collection<Element> contained = new HashSet<Element>();
        Collection<Element> notContained = new HashSet<Element>();

        contained.add(PeriodicTable.getElementBySymbol("C"));
        contained.add(PeriodicTable.getElementBySymbol("H"));
        notContained.add(PeriodicTable.getElementBySymbol("O"));
        notContained.add(PeriodicTable.getElementBySymbol("Ti"));

        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        HashMap<Variable, List<Integer>> mBalance = new HashMap<Variable, List<Integer>>();

        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(2);
        l.add(4);
        l.add(0);
        mBalance.put(vSet.getVariableOf(C2H4), l);

        l = new ArrayList<Integer>();
        l.add(2);
        l.add(6);
        l.add(0);
        mBalance.put(vSet.getVariableOf(C2H6), l);
        
        l = new ArrayList<Integer>();
        l.add(3);
        l.add(8);
        l.add(0);
        mBalance.put(vSet.getVariableOf(C3H8), l);

        assert(testMassBalance(species, contained, notContained, mBalance, vSet));
    }
    
    protected boolean testConservationTypes(ReactionType rt, Species species, Collection<String> contained) {
        boolean valid = true;
        Collection<String> consTypes = rt.getAllConservationTypes(species);
        if (contained != null) {
            valid &= contained.size() == consTypes.size();
            for (String constraint : contained) {
                valid &= consTypes.contains(constraint);
            }
        }
        return valid;
    }
    
    protected boolean testConservationTypes(ReactionType rt, Collection<Species> species, Collection<String> contained, HashMap<Variable, List<Integer>> balance, VariableSet vSet) {
        boolean valid = true;
        Collection<String> consTypes = rt.getAllConservationTypes(species);
        if (contained != null) {
            valid &= contained.size() == consTypes.size();
            for (String constraint : contained) {
                valid &= consTypes.contains(constraint);
            }
        }
        HashMap<Variable, List<Integer>> calcBalance = rt.getConservationTypeConstraints(species, vSet);
        for (Variable v1 : balance.keySet()) {
            valid &= calcBalance.containsKey(v1);
            if (valid) {
                List<Integer> l1 = calcBalance.get(v1);
                List<Integer> l2 = calcBalance.get(v1);
                valid &= l1.size() == l2.size();
                if (valid) {
                    for (int i = 0; i < l1.size(); i++) {
                        valid &= l2.contains(l1.get(i));
                    }
                }
            }
        }
        return valid;
    }
}