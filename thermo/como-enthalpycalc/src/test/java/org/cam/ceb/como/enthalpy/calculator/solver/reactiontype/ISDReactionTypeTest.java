/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver.reactiontype;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import org.cam.ceb.como.enthalpy.calculator.MockSpecies;
import org.cam.ceb.como.enthalpy.calculator.MockSpeciesConnectivity;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.Variable;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableSet;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class ISDReactionTypeTest extends ReactionTypeTest {

    @Test
    public void SingleSpeciesTest() {
        Collection<String> contained = new HashSet<String>();

        contained.add("[C]{1}[H]");

        assert (testConservationTypes(new ISDReactionType(), MockSpeciesConnectivity.getCH3(), contained));
        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getCH4(), contained));

        contained.add("[C]{1}[C]");

        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getC2H6(), contained));
        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getC3H8(), contained));

        contained.add("[C]{2}[C]");

        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getC3H6(), contained));

        contained.add("[C]{1}[O]");
        contained.add("[H]{1}[O]");
        contained.remove("[C]{2}[C]");

        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getC2H6O(), contained));

        contained.add("[C]{2}[C]");
        contained.remove("[C]{1}[C]");

        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getC2H4O(), contained));

        contained.remove("[C]{2}[C]");

        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getCH4O(), contained));

        contained.add("[C]{2}[C]");
        contained.remove("[C]{1}[O]");
        contained.remove("[H]{1}[O]");

        assert (testConservationTypes(new ISDReactionType(), MockSpecies.getC2H4(), contained));
    }
    
    @Test
    public void SpeciesSetTestCHSingle() {
        Species CH3 = MockSpeciesConnectivity.getCH3();
        Species CH4 = MockSpecies.getCH4();
        
        Collection<Species> species = new HashSet<Species>();
        species.add(CH3);
        species.add(CH4);
        
        Collection<String> contained = new HashSet<String>();
        contained.add("[C]{1}[H]");
        
        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        
        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(3);
        balance.put(vSet.getVariableOf(CH3), l);
        
        l = new ArrayList<Integer>();
        l.add(4);
        balance.put(vSet.getVariableOf(CH4), l);

        assert (testConservationTypes(new ISDReactionType(), species, contained, balance, vSet));
    }
    
    @Test
    public void SpeciesSetTestCH() {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H6 = MockSpecies.getC2H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species C3H6 = MockSpecies.getC3H6();
        
        Collection<Species> species = new HashSet<Species>();
        species.add(C2H4);
        species.add(C2H6);
        species.add(C3H8);
        species.add(C3H6);
        
        Collection<String> contained = new HashSet<String>();
        contained.add("[C]{1}[C]");
        contained.add("[C]{2}[C]");
        contained.add("[C]{1}[H]");
        
        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        
        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(4);
        balance.put(vSet.getVariableOf(C2H4), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(6);
        balance.put(vSet.getVariableOf(C2H6), l);
        
        l = new ArrayList<Integer>();
        l.add(2);
        l.add(0);
        l.add(8);
        balance.put(vSet.getVariableOf(C3H8), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(1);
        l.add(6);
        balance.put(vSet.getVariableOf(C3H6), l);

        assert (testConservationTypes(new ISDReactionType(), species, contained, balance, vSet));
    }

    @Test
    public void SpeciesSetTestCHO() {
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H8 = MockSpecies.getC3H8();
        Species C3H6 = MockSpecies.getC3H6();
        
        Collection<Species> species = new HashSet<Species>();
        species.add(C2H4);
        species.add(C2H4O);
        species.add(C2H6);
        species.add(C2H6O);
        species.add(C3H8);
        species.add(C3H6);
        
        Collection<String> contained = new HashSet<String>();
        contained.add("[C]{1}[C]");
        contained.add("[C]{2}[C]");
        contained.add("[C]{1}[H]");
        contained.add("[C]{1}[O]");
        contained.add("[H]{1}[O]");
        
        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        
        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(4);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H4), l);
        
        l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(3);
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(C2H4), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(6);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H6), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(5);
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(C2H6O), l);
        
        l = new ArrayList<Integer>();
        l.add(2);
        l.add(0);
        l.add(8);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C3H8), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(1);
        l.add(6);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C3H6), l);

        assert (testConservationTypes(new ISDReactionType(), species, contained, balance, vSet));
    }
}
