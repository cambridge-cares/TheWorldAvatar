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
public class HHDReactionTypeTest extends ReactionTypeTest {

    @Test
    public void SingleSpeciesTest() {

        // MANUAL CHECK REQUIRED!!!

        Collection<String> reactionTypes = new HashSet<String>();

        reactionTypes.add("[H]{1}[C]([H]{1})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpeciesConnectivity.getCH3(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("[H]{1}[C]([H]{1})([H]{1})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getCH4(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("([H]{1})([H]{1})[C]{2}[C]([H]{1})([H]{1})");
        reactionTypes.add("[H]{1}[C]([C]{2})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getC2H4(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        reactionTypes.add("([H]{1})([H]{1})([H]{1})[C]{1}[C]([H]{1})([H]{1})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getC2H6(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("([C]{1})([H]{1})([H]{1})[C]{1}[C]([H]{1})([H]{1})([H]{1})");
        reactionTypes.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        reactionTypes.add("[H]{1}[C]([C]{1})([C]{1})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getC3H8(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("([C]{2})([H]{1})[C]{1}[C]([H]{1})([H]{1})([H]{1})");
        reactionTypes.add("[H]{1}[C]([C]{1})([C]{2})");
        reactionTypes.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        reactionTypes.add("([C]{1})([H]{1})[C]{2}[C]([H]{1})([H]{1})");
        reactionTypes.add("[H]{1}[C]([C]{2})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getC3H6(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("[H]{1}[O]([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpeciesConnectivity.getH2O(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("[O]{2}[O]");
        assert (testConservationTypes(new HHDReactionType(), MockSpeciesConnectivity.getO2(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("[H]{1}[C]([H]{1})([H]{1})([O]{1})");
        reactionTypes.add("[H]{1}[O]([C]{1})");
        reactionTypes.add("([H]{1})[O]{1}[C]([H]{1})([H]{1})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getCH4O(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("[H]{1}[C]([C]{2})([O]{1})");
        reactionTypes.add("[H]{1}[O]([C]{1})");
        reactionTypes.add("([C]{2})([H]{1})[C]{1}[O]([H]{1})");
        reactionTypes.add("([H]{1})([H]{1})[C]{2}[C]([H]{1})([O]{1})");
        reactionTypes.add("[H]{1}[C]([C]{2})([H]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getC2H4O(), reactionTypes));

        reactionTypes = new HashSet<String>();
        reactionTypes.add("([H]{1})([H]{1})([H]{1})[C]{1}[C]([H]{1})([H]{1})([O]{1})");
        reactionTypes.add("([C]{1})([H]{1})([H]{1})[C]{1}[O]([H]{1})");
        reactionTypes.add("[H]{1}[O]([C]{1})");
        reactionTypes.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        reactionTypes.add("[H]{1}[C]([C]{1})([H]{1})([O]{1})");
        assert (testConservationTypes(new HHDReactionType(), MockSpecies.getC2H6O(), reactionTypes));
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
        contained.add("([H]{1})([H]{1})[C]{2}[C]([H]{1})([H]{1})");
        contained.add("[H]{1}[C]([C]{2})([H]{1})");

        contained.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        contained.add("([H]{1})([H]{1})([H]{1})[C]{1}[C]([H]{1})([H]{1})([H]{1})");

        contained.add("([C]{1})([H]{1})([H]{1})[C]{1}[C]([H]{1})([H]{1})([H]{1})");
        //reactionTypes.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        contained.add("[H]{1}[C]([C]{1})([C]{1})([H]{1})");

        contained.add("([C]{2})([H]{1})[C]{1}[C]([H]{1})([H]{1})([H]{1})");
        contained.add("[H]{1}[C]([C]{1})([C]{2})");
        //reactionTypes.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        contained.add("([C]{1})([H]{1})[C]{2}[C]([H]{1})([H]{1})");
        //reactionTypes.add("[H]{1}[C]([C]{2})([H]{1})");

        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));

        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H4), l);

        l = new ArrayList<Integer>();
        l.add(0);
        l.add(0);
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H6), l);

        l = new ArrayList<Integer>();
        l.add(0);
        l.add(0);
        l.add(1);
        l.add(0);
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C3H8), l);

        l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(1);
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(C3H6), l);

        assert (testConservationTypes(new HHDReactionType(), species, contained, balance, vSet));
    }

    @Test
    public void SpeciesSetTestCHO() {
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6O = MockSpecies.getC2H6O();
        Species CH4O = MockSpecies.getCH4O();

        Collection<Species> species = new HashSet<Species>();
        species.add(C2H4O);
        species.add(C2H6O);
        species.add(CH4O);

        Collection<String> contained = new HashSet<String>();
        contained.add("[H]{1}[C]([C]{2})([O]{1})");
        contained.add("[H]{1}[O]([C]{1})");
        contained.add("([C]{2})([H]{1})[C]{1}[O]([H]{1})");
        contained.add("([H]{1})([H]{1})[C]{2}[C]([H]{1})([O]{1})");
        contained.add("[H]{1}[C]([C]{2})([H]{1})");

        contained.add("([H]{1})([H]{1})([H]{1})[C]{1}[C]([H]{1})([H]{1})([O]{1})");
        contained.add("([C]{1})([H]{1})([H]{1})[C]{1}[O]([H]{1})");
        //contained.add("[H]{1}[O]([C]{1})");
        contained.add("[H]{1}[C]([C]{1})([H]{1})([H]{1})");
        contained.add("[H]{1}[C]([C]{1})([H]{1})([O]{1})");

        contained.add("[H]{1}[C]([H]{1})([H]{1})([O]{1})");
        //contained.add("[H]{1}[O]([C]{1})");
        contained.add("([H]{1})[O]{1}[C]([H]{1})([H]{1})([H]{1})");

        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));

        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(1);
        l.add(1);
        l.add(1);
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H4O), l);

        l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(1);
        l.add(1);
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H6O), l);

        l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(CH4O), l);

        assert (testConservationTypes(new HHDReactionType(), species, contained, balance, vSet));
    }
}
