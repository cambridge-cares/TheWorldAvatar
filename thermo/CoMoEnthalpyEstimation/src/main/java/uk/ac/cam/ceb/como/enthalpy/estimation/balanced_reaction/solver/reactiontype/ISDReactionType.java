/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype;

import com.google.common.collect.Multiset;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 */
public class ISDReactionType extends ReactionType {

    @Override
    public Collection<String> getAllConservationTypes(Species species) {
        Set<String> elements = new HashSet<String>();
        for (String b : species.getBondTypeMultiset().elementSet()) {
            elements.add(b);
        }
        return elements;
    }

    @Override
    public Collection<String> getAllConservationTypes(Collection<Species> species) {
        Set<String> elements = new HashSet<String>();
        for (Species sp : species) {
            for (String b : sp.getBondTypeMultiset().elementSet()) {
                elements.add(b);
            }
        }
        return elements;
    }

    @Override
    public HashMap<Variable, List<Integer>> getConservationTypeConstraints(Collection<Species> species, VariableSet vSet) {
        Collection<String> bondTypes = getAllConservationTypes(species);
        return getConservationConstraints(species, vSet, bondTypes, new ISDConservationConstraintMultisetSelector());
    }

    protected class ISDConservationConstraintMultisetSelector implements MultisetSelector {

        @Override
        public Multiset getMultiset(Species sp) {
            return sp.getBondTypeMultiset();
        }
    }
}
