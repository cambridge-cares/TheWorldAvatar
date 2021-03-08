/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.filter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.OutlierReactionSelector;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.MPSFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.reactiontype.HHDReactionType;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.validation.LeaveOneOutCrossValidation;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.PoolModificationCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;

/**
 *
 *  import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
 *  
 */

/**
 *
 * @author pb556
 * 
 */
public class ValidationFilter extends SpeciesFilter {

    protected int numResults = 25;
    
    protected double maxErr = 20;
    
    protected Map<Species, Reaction> detValidSpecies = new HashMap<Species, Reaction>();
    
    protected Map<Species, Reaction> detInvalidSpecies = new HashMap<Species, Reaction>();

    @Override
    public Collection<Species> filter() {
    	
        detValidSpecies = new HashMap<Species, Reaction>();
        
        detInvalidSpecies = new HashMap<Species, Reaction>();
        
        identify();
        
        return validSpecies;
    }

    @Override
    public Collection<Species> filter(Collection<Species> species) {
    	
        this.species = species;
        
        return filter();
        
    }
    
    public Map<Species, Reaction> getDetailedValidSpecies() {
    	
        return detValidSpecies;
        
    }
    
    public Map<Species, Reaction> getDetailedInvalidSpecies() {
    	
        return detInvalidSpecies;
        
    }

    protected void identify() {
    	
        LeaveOneOutCrossValidation validation = new LeaveOneOutCrossValidation(getPool(),new PoolModificationCalculator(numResults, new TerminalGLPKSolver(true, true), new MPSFormat(false, new HHDReactionType())),new OutlierReactionSelector());
    	
        validation.validate();

        /**
         * 
         * "res" remembers species and difference between enthalpy given in table and estimated enthalpy.
         *   
         */
        Map<Species, Double> res = validation.getValidationResults();
        
        Map<Species, Reaction> detRes = validation.getDetailedValidationResults();
        
        invalidSpecies = new ArrayList<Species>();
        
        validSpecies = new ArrayList<Species>();

        for (Species s : res.keySet()) {
        	
//        System.out.println("protected void identify(): Math.abs(res.get(s)) > maxErr:  " + res.get(s));
        
        if (Math.abs(res.get(s)) > maxErr) {
            	
                invalidSpecies.add(s);
                
                detInvalidSpecies.put(s, detRes.get(s));
                
            } else {
            	
                validSpecies.add(s);
                
                detValidSpecies.put(s, detRes.get(s));
            }
        }
    }

    protected ObjectPool<Species> getPool() {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        if (species != null && !species.isEmpty()) {
            for (Species s : species) {
                pool.addValidated(s);
            }
        }
        return pool;
    }
}
