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
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation.LeaveOneOutCrossValidation;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.ObjectPoolCalculator;

/**
 *
 * @author pb556
 * 
 */
public class ReactionInfluenceFilterSimpleExclusion extends SpeciesFilter {

    protected double maxErr = 25;
    protected int numResults = 25;
    protected ObjectPoolCalculator calculator = null;
    protected ReactionSelector selector = null;
    //protected double maxCtr = 5;
    protected Map<Species, Reaction> detValidSpecies = new HashMap<Species, Reaction>();
    protected Map<Species, Reaction> detInvalidSpecies = new HashMap<Species, Reaction>();

    public ReactionInfluenceFilterSimpleExclusion(ObjectPoolCalculator calculator, ReactionSelector selector, double maxErr) {
        super();
        this.maxErr = maxErr;
        this.selector = selector;
        this.calculator = calculator;
       // this.maxCtr = maxCtr;
    }
    
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
        boolean execute = true;

        invalidSpecies = new ArrayList<Species>();
        validSpecies = new ArrayList<Species>();

        while (execute) {
        	
            execute = false;

            LeaveOneOutCrossValidation validation = new LeaveOneOutCrossValidation(getPool(invalidSpecies),calculator,selector);
            
            validation.validate();
            
            Map<Species, Reaction> res = validation.getDetailedValidationResults();
            Map<Species, Reaction> acceptableRes = new HashMap<Species, Reaction>();
            Map<Species, Reaction> inacceptableRes = new HashMap<Species, Reaction>();
            for (Species s : res.keySet()) {
                if (Math.abs(s.getHf() - res.get(s).calculateHf()) > maxErr) {
                    inacceptableRes.put(s, res.get(s));
                    invalidSpecies.add(s);
                    detInvalidSpecies.put(s, res.get(s));
                    execute = true;
                    System.out.println(s.getRef() + "(" + Math.abs(s.getHf() - res.get(s).calculateHf()) + "): " + res.get(s).toString());
                } else {
                    acceptableRes.put(s, res.get(s));
                    validSpecies.add(s);
                    detValidSpecies.put(s, res.get(s));
                }
            }

            if (execute) {
                validSpecies.clear();
                detValidSpecies.clear();
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

    protected ObjectPool getPool(Species ignore) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        if (species != null && !species.isEmpty()) {
            for (Species s : species) {
                if (!ignore.equals(s, true)) {
                    pool.addValidated(s);
                }
            }
        }
        return pool;
    }

    protected ObjectPool getPool(Collection<Species> ignore) {
        ObjectPool<Species> pool = new ObjectPool<Species>();
        if (species != null && !species.isEmpty()) {
            for (Species s : species) {
                boolean valid = true;
                for (Species v : ignore) {
                    if (v.equals(s, true)) {
                        valid = false;
                        break;
                    }
                }
                if (valid) {
                    pool.addValidated(s);
                }
            }
            return pool;
        }
        return getPool();
    }
}
