/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.HODReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ReactionType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpecies;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.MockSpeciesConnectivity;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;
import org.junit.Test;

/**
 *
 * @author pb556
 */
public class HODReactionTypeTest extends ReactionTypeTest {

    @Test
    public void print() {

        //for (Species sp : MockSpecies.getPool()) {

        Species sp = MockSpecies.getC3H6();

        System.out.println(sp.getRef());

        Collection<Species> species = new HashSet<Species>();
        species.add(sp);

        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        vSet.getVariableOf(sp);

        ReactionType rt = new HODReactionType();
        Collection<String> types = rt.getAllConservationTypes(sp);
        for (String str : types) {
            System.out.println(str);
        }
        HashMap<Variable, List<Integer>> s = rt.getConservationTypeConstraints(species, vSet);
        for (Variable str : s.keySet()) {
            for (int i = 0; i < s.get(str).size(); i++) {
                System.out.println(str.name + " " + (i + 1) + " = " + s.get(str).get(i));
            }
        }
        //}
    }

    @Test
    public void SingleSpeciesTest() {
        Collection<String> contained = new HashSet<String>();

        contained.add("[C]([H]{1})([H]{1})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpeciesConnectivity.getCH3(), contained));

        contained.remove("[C]([H]{1})([H]{1})([H]{1})");
        contained.add("[C]([H]{1})([H]{1})([H]{1})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getCH4(), contained));

        contained.remove("[C]([H]{1})([H]{1})([H]{1})([H]{1})");
        contained.add("[C]([C]{1})([H]{1})([H]{1})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getC2H6(), contained));

        contained.add("[C]([C]{1})([C]{1})([H]{1})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getC3H8(), contained));

        contained.remove("[C]([C]{1})([C]{1})([H]{1})([H]{1})");
        contained.add("[C]([C]{2})([H]{1})([H]{1})");
        contained.add("[C]([C]{1})([C]{2})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getC3H6(), contained));

        contained.remove("[C]([C]{1})([H]{1})([H]{1})([H]{1})");
        contained.remove("[C]([C]{1})([C]{2})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getC2H4(), contained));

        contained.remove("[C]([C]{2})([H]{1})([H]{1})");

        contained.add("[O]([C]{1})([H]{1})");
        contained.add("[C]([H]{1})([H]{1})([H]{1})([O]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getCH4O(), contained));

        contained.remove("[C]([H]{1})([H]{1})([H]{1})([O]{1})");
        contained.remove("[C]([H]{1})([H]{1})([H]{1})([O]{1})");
        contained.add("[C]([C]{1})([H]{1})([H]{1})([O]{1})");
        contained.add("[C]([C]{1})([H]{1})([H]{1})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getC2H6O(), contained));

        contained.remove("[C]([C]{1})([H]{1})([H]{1})([H]{1})");
        contained.remove("[O]([C]{1})([H]{1})");
        contained.remove("[C]([C]{1})([H]{1})([H]{1})([O]{1})");
        contained.add("[O]([C]{1})([H]{1})");
        contained.add("[C]([C]{2})([H]{1})([O]{1})");
        contained.add("[C]([C]{2})([H]{1})([H]{1})");

        assert (testConservationTypes(new HODReactionType(), MockSpecies.getC2H4O(), contained));

    }

    @Test
    public void SpeciesSetTestCH() {
        Species C2H6 = MockSpecies.getC2H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species C3H6 = MockSpecies.getC3H6();
        Species C2H4 = MockSpecies.getC2H4();

        Collection<Species> species = new HashSet<Species>();
        species.add(C2H6);
        species.add(C3H8);
        species.add(C3H6);
        species.add(C2H4);

        Collection<String> contained = new HashSet<String>();
        contained.add("[C]([C]{1})([H]{1})([H]{1})([H]{1})");
        contained.add("[C]([C]{1})([C]{1})([H]{1})([H]{1})");
        contained.add("[C]([C]{2})([H]{1})([H]{1})");
        contained.add("[C]([C]{1})([C]{2})([H]{1})");

        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));

        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(2);
        l.add(0);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H6), l);

        l = new ArrayList<Integer>();
        l.add(2);
        l.add(1);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C3H8), l);

        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(C3H6), l);

        l = new ArrayList<Integer>();
        l.add(0);
        l.add(0);
        l.add(2);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H4), l);

        assert (testConservationTypes(new HODReactionType(), species, contained, balance, vSet));
    }

    @Test
    public void SpeciesSetTestCHO() {
        Species CH4O = MockSpecies.getCH4O();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6O = MockSpecies.getC2H6O();

        Collection<Species> species = new HashSet<Species>();
        species.add(CH4O);
        species.add(C2H4O);
        species.add(C2H6O);

        Collection<String> contained = new HashSet<String>();
        contained.add("[O]([C]{1})([H]{1})");
        contained.add("[C]([H]{1})([H]{1})([H]{1})([O]{1})");
        contained.add("[C]([C]{1})([H]{1})([H]{1})([O]{1})");
        contained.add("[C]([C]{1})([H]{1})([H]{1})([H]{1})");
        contained.add("[C]([C]{2})([H]{1})([O]{1})");
        contained.add("[C]([C]{2})([H]{1})([H]{1})");

        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));

        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(CH4O), l);

        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(1);
        l.add(1);
        l.add(0);
        l.add(0);
        balance.put(vSet.getVariableOf(C2H4O), l);

        l = new ArrayList<Integer>();
        l.add(1);
        l.add(0);
        l.add(0);
        l.add(0);
        l.add(1);
        l.add(1);
        balance.put(vSet.getVariableOf(C2H6O), l);

        assert (testConservationTypes(new HODReactionType(), species, contained, balance, vSet));
    }
}
