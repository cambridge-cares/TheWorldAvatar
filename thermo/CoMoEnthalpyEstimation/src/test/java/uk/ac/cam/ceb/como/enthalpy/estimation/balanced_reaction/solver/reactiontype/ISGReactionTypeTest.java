/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISGReactionType;
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
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 * @author pb556
 */

public class ISGReactionTypeTest extends ReactionTypeTest {

    // Manual check required - OK
    @Test
    @Ignore
    public void print() {

        for (Species sp : MockSpecies.getPool()) {

            Collection<Species> species = new HashSet<Species>();
            species.add(sp);

            VariableSet vSet = new VariableSet(new VariableFactory("v"));
            vSet.getVariableOf(sp);

            ReactionType rt = new ISGReactionType(true);
            HashMap<Variable, List<Integer>> s = rt.getConservationTypeConstraints(species, vSet);

            System.out.println(sp.getRef());
            for (Variable str : s.keySet()) {
                System.out.println("paired = " + s.get(str).get(0));
                System.out.println("unpaired = " + s.get(str).get(1));
            }
        }
    }

    @Test
    @Ignore
    public void print2() {
        Species sp = MockSpeciesConnectivity.getCH3();
        
        Collection<Species> species = new HashSet<Species>();
        species.add(sp);

        VariableSet vSet = new VariableSet(new VariableFactory("v"));
        vSet.getVariableOf(sp);

        ReactionType rt = new ISGReactionType(true);
        HashMap<Variable, List<Integer>> s = rt.getConservationTypeConstraints(species, vSet);

        System.out.println(sp.getRef());
        for (Variable str : s.keySet()) {
            System.out.println("paired = " + s.get(str).get(0));
            System.out.println("unpaired = " + s.get(str).get(1));
        }
    }

    @Test
    public void SingleSpeciesTest() {
        Collection<String> contained = new HashSet<String>();
        contained.add("{paired}");

        assert (testConservationTypes(new ISGReactionType(false), MockSpeciesConnectivity.getCH3(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getCH4(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getC2H4(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getC2H4O(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getC2H6(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getC2H6O(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getC3H6(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getC3H8(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpecies.getCH4O(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpeciesConnectivity.getH(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpeciesConnectivity.getH2O(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpeciesConnectivity.getO(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpeciesConnectivity.getO2(), contained));
        assert (testConservationTypes(new ISGReactionType(false), MockSpeciesConnectivity.getOH(), contained));

        contained.add("{unpaired}");

        assert (testConservationTypes(new ISGReactionType(true), MockSpeciesConnectivity.getCH3(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getCH4(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getC2H4(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getC2H4O(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getC2H6(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getC2H6O(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getC3H6(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getC3H8(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpecies.getCH4O(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpeciesConnectivity.getH(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpeciesConnectivity.getH2O(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpeciesConnectivity.getO(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpeciesConnectivity.getO2(), contained));
        assert (testConservationTypes(new ISGReactionType(true), MockSpeciesConnectivity.getOH(), contained));
    }

    @Test
    public void SpeciesSetTestCHSingle() {
        Species CH3 = MockSpeciesConnectivity.getCH3();
        Species CH4 = MockSpecies.getCH4();
        Species C2H4 = MockSpecies.getC2H4();
        Species C2H4O = MockSpecies.getC2H4O();
        Species C2H6 = MockSpecies.getC2H6();
        Species C2H6O = MockSpecies.getC2H6O();
        Species C3H6 = MockSpecies.getC3H6();
        Species C3H8 = MockSpecies.getC3H8();
        Species CH4O = MockSpecies.getCH4O();
        Species H = MockSpeciesConnectivity.getH();
        Species H2O = MockSpeciesConnectivity.getH2O();
        Species O = MockSpeciesConnectivity.getO();
        Species O2 = MockSpeciesConnectivity.getO2();
        Species OH = MockSpeciesConnectivity.getOH();

        Collection<Species> species = new HashSet<Species>();
        species.add(CH3);
        species.add(CH4);
        species.add(C2H4);
        species.add(C2H4O);
        species.add(C2H6);
        species.add(C2H6O);
        species.add(C3H6);
        species.add(C3H8);
        species.add(CH4O);
        species.add(H);
        species.add(H2O);
        species.add(O);
        species.add(O2);
        species.add(OH);

        Collection<String> contained = new HashSet<String>();
        contained.add("{paired}");
        contained.add("{unpaired}");

        HashMap<Variable, List<Integer>> balance = new HashMap<Variable, List<Integer>>();
        VariableSet vSet = new VariableSet(new VariableFactory("v"));

        ArrayList<Integer> l = new ArrayList<Integer>();
        l.add(8);
        l.add(1);
        balance.put(vSet.getVariableOf(CH3), l);

        l = new ArrayList<Integer>();
        l.add(8);
        l.add(2);
        balance.put(vSet.getVariableOf(CH4), l);

        l = new ArrayList<Integer>();
        l.add(12);
        l.add(4);
        balance.put(vSet.getVariableOf(C2H4), l);

        l = new ArrayList<Integer>();
        l.add(20);
        l.add(4);
        balance.put(vSet.getVariableOf(C2H4O), l);

        l = new ArrayList<Integer>();
        l.add(14);
        l.add(4);
        balance.put(vSet.getVariableOf(C2H6), l);

        l = new ArrayList<Integer>();
        l.add(22);
        l.add(4);
        balance.put(vSet.getVariableOf(C2H6O), l);

        l = new ArrayList<Integer>();
        l.add(18);
        l.add(6);
        balance.put(vSet.getVariableOf(C3H6), l);

        l = new ArrayList<Integer>();
        l.add(20);
        l.add(6);
        balance.put(vSet.getVariableOf(C3H8), l);

        l = new ArrayList<Integer>();
        l.add(16);
        l.add(2);
        balance.put(vSet.getVariableOf(CH4O), l);

        l = new ArrayList<Integer>();
        l.add(0);
        l.add(1);
        balance.put(vSet.getVariableOf(H), l);

        l = new ArrayList<Integer>();
        l.add(10);
        l.add(0);
        balance.put(vSet.getVariableOf(H2O), l);

        l = new ArrayList<Integer>();
        l.add(6);
        l.add(2);
        balance.put(vSet.getVariableOf(O), l);

        l = new ArrayList<Integer>();
        l.add(16);
        l.add(0);
        balance.put(vSet.getVariableOf(O2), l);

        l = new ArrayList<Integer>();
        l.add(8);
        l.add(1);
        balance.put(vSet.getVariableOf(OH), l);

        assert (testConservationTypes(new ISGReactionType(true), species, contained, balance, vSet));
    }
}
