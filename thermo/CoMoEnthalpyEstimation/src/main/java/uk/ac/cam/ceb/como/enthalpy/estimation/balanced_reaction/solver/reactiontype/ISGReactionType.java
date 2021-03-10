/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.SolverHelper;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.Variable;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 * 
 */

public class ISGReactionType extends ReactionType {

    protected boolean includeUnpaired = true;

    public ISGReactionType(boolean includeUnpaired) {
    	
    this.includeUnpaired = includeUnpaired;
        
    }

    @Override
    public Collection<String> getAllConservationTypes(Collection<Species> species) {
    	
        ArrayList<String> l = new ArrayList<String>();
        
        l.add("{paired}");
        
        if (includeUnpaired) {
        	
            l.add("{unpaired}");
            
        }
        
        return l;
    }

    @Override
    public LinkedHashMap<Variable, List<Integer>> getConservationTypeConstraints(Collection<Species> species, VariableSet vSet) {
    	
        LinkedHashMap<Variable, Integer> paired = getElectronenPairConvervationConstraints(species, vSet, true);
        
        LinkedHashMap<Variable, Integer> unpaired = getElectronenPairConvervationConstraints(species, vSet, false);
        
        LinkedHashMap<Variable, List<Integer>> combined = new LinkedHashMap<Variable, List<Integer>>();
        
        if (paired.size() != unpaired.size()) {
        	
            System.out.println("ERROR");
            
            return null;
        }

        for (Variable v : paired.keySet()) {
        	
            ArrayList<Integer> l = new ArrayList<Integer>();
            
            if (!unpaired.containsKey(v)) {
            	
                System.out.println("ERROR");
                return null;
            }
            
            l.add(paired.get(v));
            if (includeUnpaired) {
            l.add(unpaired.get(v));
            
            }
            
            combined.put(v, l);
        }
        
        return combined;
    }

    @Override
    public Collection<String> getAllConservationTypes(Species species) {
    	
        ArrayList<String> l = new ArrayList<String>();
        
        l.add("{paired}");
        
        if (includeUnpaired) {
        	
            l.add("{unpaired}");
            
        }
        
        return l;
    }

    protected LinkedHashMap<Variable, Integer> getElectronenPairConvervationConstraints(Collection<Species> species, VariableSet vSet, boolean paired) {
    	
        LinkedHashMap<Variable, Integer> constraintBalance = new LinkedHashMap<Variable, Integer>();
        
        //ISGMPSFormat1.MultisetSelector ms = new ISGMPSFormat1.AtomMultisetSelector();
        //logger.trace("Writing convervation equation for constraint of type : " + constraint);
        // make a string of each term
        
        for (Iterator<Species> it = species.iterator(); it.hasNext();) {
        	
            Species sp = it.next();
            
            Variable variable = vSet.getVariableOf(sp);
            
            int count = SolverHelper.getNumberOfPairedElectrons(sp);
            
            if (!paired) {
            	
            count = SolverHelper.getNumberOfUnpairedElectrons(sp);
            
            }
            
            constraintBalance.put(variable, count);
        
        }
        
        return constraintBalance;
        
    }
}