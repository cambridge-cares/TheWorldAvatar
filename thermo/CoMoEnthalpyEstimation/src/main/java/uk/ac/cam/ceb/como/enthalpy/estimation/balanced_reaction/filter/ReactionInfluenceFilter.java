/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.OutlierReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
//import uk.ac.cam.ceb.como.enthalpy.calculator.solver.glpk.ISDMPSDoubleFormat;
//import uk.ac.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation.LeaveOneOutCrossValidation;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;

/**
 *
 * @author pb556
 */
public class ReactionInfluenceFilter extends SpeciesFilter {

    protected LPSolver solver;
    protected LPFormat format;
    protected double maxErr = 25;
    protected int numResults = 25;
    //protected double maxCtr = 5;
    protected Map<Species, Reaction> detValidSpecies = new HashMap<Species, Reaction>();
    protected Map<Species, Reaction> detInvalidSpecies = new HashMap<Species, Reaction>();

    public ReactionInfluenceFilter(LPSolver solver, LPFormat format, double maxErr) {
        super();
        this.solver = solver;
        this.format = format;
        this.maxErr = maxErr;
        //this.maxCtr = maxCtr;
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
        
        // Check if still all species can be calculated!
        // exclude only species which can be created by others
        // mark essential species which give bad results
        
        invalidSpecies = new ArrayList<Species>();
        validSpecies = new ArrayList<Species>();
        while (execute) {
            execute = false;
            LeaveOneOutCrossValidation validation = new LeaveOneOutCrossValidation(
                    getPool(invalidSpecies),
                    new PoolModificationCalculator(numResults, solver, format),
                    new OutlierReactionSelector());
            validation.validate();

            Map<Species, Reaction> res = validation.getDetailedValidationResults();
            Map<Species, Reaction> acceptableRes = new HashMap<Species, Reaction>();
            Map<Species, Reaction> inacceptableRes = new HashMap<Species, Reaction>();
            for (Species s : res.keySet()) {
                if (Math.abs(s.getHf() - res.get(s).calculateHf()) > maxErr) {
                    inacceptableRes.put(s, res.get(s));
                } else {
                    acceptableRes.put(s, res.get(s));
                    if (Math.abs(s.getHf() - res.get(s).calculateHf()) <= 2) {
                        validSpecies.addAll(res.get(s).getProducts().keySet());
                        validSpecies.addAll(res.get(s).getReactants().keySet());
                    }
                }
            }

            // identification of bad species
            Map<Species, Integer> negCtr = count(inacceptableRes);
            Map<Species, Integer> posCtr = count(acceptableRes);

            for (Species s : negCtr.keySet()) {
                Species resKey = null;
                for (Species resSp : res.keySet()) {
                    if (resSp.equals(s, true)) {
                        resKey = resSp;
                        break;
                    }
                }

                boolean next = false;
                for (Species k : validSpecies) {
                    if (k.equals(s, true)) {
                        next = true;
                        break;
                    }
                }
                if (next) {
                    continue;
                }

                Species key = null;
                for (Species k : posCtr.keySet()) {
                    if (s.equals(k, true)) {
                        key = k;
                        break;
                    }
                }
                if (key != null) {
                    if (verifyImprovement(s, res)) {
                        if (resKey != null) {
                            invalidSpecies.add(resKey);
                            detInvalidSpecies.put(resKey, res.get(resKey));
                        }
                        execute = true;
                    }
                } else {
                    if (resKey != null) {
                        invalidSpecies.add(resKey);
                        detInvalidSpecies.put(resKey, res.get(resKey));
                    }
                    execute = true;
                }
            }

            if (!execute) {
                // create the detailed results
                for (Species k : res.keySet()) {
                    boolean valid = false;
                    for (Species s : validSpecies) {
                        if (k.equals(s, true)) {
                            detValidSpecies.put(s, res.get(k));
                            valid = true;
                            break;
                        }
                    }
                    if (!valid) {
                        valid = true;
                        for (Species s : invalidSpecies) {
                            if (k.equals(s, true)) {
                                valid = false;
                                break;
                            }
                        }
                        if (valid) {
                            validSpecies.add(k);
                            detValidSpecies.put(k, res.get(k));
                        }
                    }
                }
            }
        }
    }

    // use reactions in which the species was involved
    protected boolean verifyObtainedResults(Species s, Map<Species, Reaction> res) {
        PoolModificationCalculator calc = new PoolModificationCalculator(numResults, solver, format, getPool(s));
        try {
            calc.calculate(res.keySet());
        } catch (Exception ex) {
            Logger.getLogger(ReactionInfluenceFilterMarking.class.getName()).log(Level.SEVERE, null, ex);
            return false;
        }
        return true;
    }

    // use reactions in which the species was involved
    protected boolean verifyImprovement(Species s, Map<Species, Reaction> res) {
        // simply use a multicalculator - speeds things up!!!
        Collection<Reaction> involvedReactions = extractReactions(s, res);
        ArrayList<Species> rSpecies = new ArrayList<Species>();
        for (Reaction r : involvedReactions) {
            rSpecies.add(r.getSpecies());
        }

        double sum = 0;
        int ctr = 0;
        HashMap<Species, Reaction> results = new HashMap<Species, Reaction>();
        for (Species rS : rSpecies) {
            ArrayList<Species> ignore = new ArrayList<Species>();
            ignore.add(s);
            ignore.add(rS);
            ObjectPool<Species> pool = getPool(ignore);
            PoolModificationCalculator calc = new PoolModificationCalculator(numResults, solver, format, pool);
            try {
                calc.calculate(rS);
                HashMap<Species, ReactionList> sol = (HashMap<Species, ReactionList>) calc.get();
                for (Species sp : sol.keySet()) {
                    OutlierReactionSelector selector = new OutlierReactionSelector();
                    Reaction selR = selector.select(sol.get(sp)).get(0);
                    results.put(sp, selR);
                    sum += Math.abs(selR.getSpecies().getHf() - selR.calculateHf());
                    ctr++;
                }
            } catch (Exception ex) {
                Logger.getLogger(ReactionInfluenceFilterMarking.class.getName()).log(Level.SEVERE, null, ex);
                return false;
            }
        }

        return avgError(s, res) - 5 > (sum / ctr);

//        LeaveOneOutCrossValidation validation = new LeaveOneOutCrossValidation(
//                getPool(s),
//                new MultiCalculator(new TerminalGLPKSolver(true, true), new ISDMPSDoubleFormat()),
//                new OutlierReactionSelector());
//        validation.validate();
//        return avgError(s, res) - 5 > avgError(extractReactions(s, res), validation.getDetailedValidationResults());
    }

    protected Collection<Reaction> extractReactions(Species s, Map<Species, Reaction> res) {
        ArrayList<Reaction> list = new ArrayList<Reaction>();
        for (Species sKey : res.keySet()) {
            Reaction r = res.get(sKey);
            if (r.contains(s)) {
                list.add(r);
            }
        }
        return list;
    }

    protected double avgError(Species s, Map<Species, Reaction> res) {
        double sum = 0;
        int ctr = 0;
        for (Species sKey : res.keySet()) {
            Reaction r = res.get(sKey);
            if (r.contains(s)) {
                sum += Math.abs(r.getSpecies().getHf() - r.calculateHf());
                ctr++;
            }
        }
        return sum / ctr;
    }

    protected double avgError(Collection<Reaction> reactions, Map<Species, Reaction> res) {
        double sum = 0;
        int ctr = 0;
        if (res == null || res.isEmpty()) {
            return Integer.MAX_VALUE;
        }
        for (Reaction reaction : reactions) {
            for (Species sKey : res.keySet()) {
                Reaction r = res.get(sKey);
                if (r.getSpecies().equals(reaction.getSpecies(), true)) {
                    sum += Math.abs(r.getSpecies().getHf() - r.calculateHf());
                    ctr++;
                }
            }
        }
        return sum / ctr;
    }

    protected Map<Species, Integer> count(Map<Species, Reaction> res) {
        Map<Species, Integer> ctr = new HashMap<Species, Integer>();
        for (Species s : res.keySet()) {
            Reaction r = res.get(s);
            for (Species reactant : r.getReactants().keySet()) {
                if (reactant.equals(s, true)) {
                    continue;
                }
                Species key = null;
                for (Species k : ctr.keySet()) {
                    if (k.equals(reactant, true)) {
                        key = k;
                        break;
                    }
                }
                if (key != null) {
                    ctr.put(key, ctr.get(key) + 1);
                } else {
                    ctr.put(reactant, 1);
                }
            }
            for (Species products : r.getProducts().keySet()) {
                if (products.equals(s, true)) {
                    continue;
                }
                Species key = null;
                for (Species k : ctr.keySet()) {
                    if (k.equals(products, true)) {
                        key = k;
                        break;
                    }
                }
                if (key != null) {
                    ctr.put(key, ctr.get(key) + 1);
                } else {
                    ctr.put(products, 1);
                }
            }
        }
        return ctr;
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
