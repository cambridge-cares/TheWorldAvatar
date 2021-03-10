/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;
import uk.ac.cam.ceb.como.chem.periodictable.Element;

/**
 *
 * @author pb556
 */
public abstract class ReactionType {
    
    public abstract Collection<String> getAllConservationTypes(Species species);
    
    public abstract Collection<String> getAllConservationTypes(Collection<Species> species);
    
    public HashMap<Variable, List<Integer>> getMassBalanceConstraints(Collection<Species> species, VariableSet vSet) {
        Collection<String> elements = SolverHelper.getAllElementSymbols(species);
        return getConservationConstraints(species, vSet, elements, new AtomMultisetSelector());
    }
    
    public abstract HashMap<Variable, List<Integer>> getConservationTypeConstraints(Collection<Species> species, VariableSet vSet);
    
    protected HashMap<Variable, List<Integer>> getConservationConstraints(
            Collection<Species> species, VariableSet vSet, 
            Collection<String> constraintSet, MultisetSelector ms) {
        
        HashMap<Variable, List<Integer>> constraintBalance = new HashMap<Variable, List<Integer>>();
        int i = 0;
        for (Object constraint : constraintSet) {
            //logger.trace("Writing convervation equation for constraint of type : " + constraint);
            // make a string of each term
            for (Iterator<Species> it = species.iterator(); it.hasNext();) {
                Species sp = it.next();
                Variable variable = vSet.getVariableOf(sp);
                int count = ms.getMultiset(sp).count(constraint);
                if (!constraintBalance.containsKey(variable)) {
                    constraintBalance.put(variable, new ArrayList<Integer>());
                }
                if (constraintBalance.get(variable).size() > i) {
                    constraintBalance.get(variable).set(i, count);
                } else {
                    constraintBalance.get(variable).add(count);
                }
            }
            i++;
        }
        return constraintBalance;
    }
    
    public Collection<Element> getAllElements(Species species) {
        return SolverHelper.getAllElements(species);
    }
    
    public Collection<String> getAllElementSymbols(Species species) {
        return SolverHelper.getAllElementSymbols(species);
    }
    
    public Collection<Element> getAllElements(Collection<Species> species) {
        return SolverHelper.getAllElements(species);
    }
    
    public Collection<String> getAllElementSymbols(Collection<Species> species) {
        return SolverHelper.getAllElementSymbols(species);
    }
    
    public Collection<Element> getAllElements(Species targetSpecies, Collection<Species> refSpecies) {
        return SolverHelper.getAllElements(SolverHelper.combine(targetSpecies, refSpecies));
    }
    
    public Collection<String> getAllElementSymbols(Species targetSpecies, Collection<Species> refSpecies) {
        return SolverHelper.getAllElementSymbols(SolverHelper.combine(targetSpecies, refSpecies));
    }
    
    protected interface MultisetSelector {
        public Multiset getMultiset(Species sp);
    }
    
    protected class AtomMultisetSelector implements MultisetSelector {

        @Override
        public Multiset<String> getMultiset(Species sp) {
            Multiset<String> ms = HashMultiset.create();
            for(Element e : sp.getAtomMultiset()) {
                //if (!ms.contains(e.getSymbol())) {
                    ms.add(e.getSymbol(), 1);
                
                //}
            }
            return ms;
        }
    }
}
