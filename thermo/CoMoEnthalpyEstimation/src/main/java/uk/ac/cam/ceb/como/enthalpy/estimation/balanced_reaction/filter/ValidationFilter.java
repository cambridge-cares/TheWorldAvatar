/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.OutlierReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.MPSFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.reactiontype.ISDReactionType;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation.LeaveOneOutCrossValidation;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;

/**
 *
 * @author pb556
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
    	
        LeaveOneOutCrossValidation validation = new LeaveOneOutCrossValidation(getPool(),new PoolModificationCalculator(numResults, new TerminalGLPKSolver(true, true), new MPSFormat(false, new ISDReactionType())),new OutlierReactionSelector());
        
        validation.validate();

        Map<Species, Double> res = validation.getValidationResults();
        
        Map<Species, Reaction> detRes = validation.getDetailedValidationResults();
        
        invalidSpecies = new ArrayList<Species>();
        validSpecies = new ArrayList<Species>();

        for (Species s : res.keySet()) {
        	
        if (Math.abs(res.get(s)) > maxErr) {
            	
                invalidSpecies.add(s);
                detInvalidSpecies.put(s, detRes.get(s));
                
            } else {
            	
                validSpecies.add(s);
                detValidSpecies.put(s, detRes.get(s));
                
            }
        }
    }

    protected ObjectPool getPool() {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        if (species != null && !species.isEmpty()) {
            for (Species s : species) {
                pool.addValidated(s);
            }
        }
        return pool;
    }
}
