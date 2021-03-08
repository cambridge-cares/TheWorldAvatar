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
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.Variable;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableSet;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class HDReactionTypeTest extends ReactionTypeTest {
    
    @Test
    public void SingleSpeciesTest() {
        Collection<String> contained = new HashSet<String>();

        contained.add("{3}[C]{1}[C]{3}");

        assert (testConservationTypes(new HDReactionType(), MockSpecies.getC2H6(), contained));
        assert (testConservationTypes(new HDReactionType(), MockSpecies.getC3H8(), contained));

        contained.add("{2}[C]{2}[C]{2}");

        assert (testConservationTypes(new HDReactionType(), MockSpecies.getC3H6(), contained));

        contained.add("{3}[C]{1}[O]{1}");
        contained.remove("{2}[C]{2}[C]{2}");

        assert (testConservationTypes(new HDReactionType(), MockSpecies.getC2H6O(), contained));

        contained.add("{2}[C]{2}[C]{2}");
        contained.remove("{3}[C]{1}[C]{3}");

        assert (testConservationTypes(new HDReactionType(), MockSpecies.getC2H4O(), contained));

        contained.remove("{2}[C]{2}[C]{2}");

        assert (testConservationTypes(new HDReactionType(), MockSpecies.getCH4O(), contained));

        contained.add("{2}[C]{2}[C]{2}");
        contained.remove("{3}[C]{1}[O]{1}");

        assert (testConservationTypes(new HDReactionType(), MockSpecies.getC2H4(), contained));
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
        contained.add("{3}[C]{1}[C]{3}");
        contained.add("{2}[C]{2}[C]{2}");
        
        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        
        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        balance.put(vSet.getVariableOf(C2H4), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H6), l);
        
        l = new ArrayList<Integer>();
        l.add(2);
        l.add(0);
        balance.put(vSet.getVariableOf(C3H8), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(C3H6), l);

        assert (testConservationTypes(new HDReactionType(), species, contained, balance, vSet));
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
        contained.add("{3}[C]{1}[C]{3}");
        contained.add("{2}[C]{2}[C]{2}");
        contained.add("{3}[C]{1}[O]{1}");
        
        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        
        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H4), l);
        
        l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(C2H4), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H6), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(1);
        balance.put(vSet.getVariableOf(C2H6O), l);
        
        l = new ArrayList<Integer>();
        l.add(2);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C3H8), l);
        
        l = new ArrayList<Integer>();
        l.add(1);
        l.add(1);
        l.add(0);
        balance.put(vSet.getVariableOf(C3H6), l);

        assert (testConservationTypes(new HDReactionType(), species, contained, balance, vSet));
    }
}
