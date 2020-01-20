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
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.OutlierReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
//import uk.ac.cam.ceb.como.enthalpy.calculator.solver.glpk.ISDMPSDoubleFormat;
//import uk.ac.cam.ceb.como.enthalpy.calculator.solver.glpk.ISDMPSIntegerFormat;
//import uk.ac.cam.ceb.como.enthalpy.calculator.solver.glpk.TerminalGLPKSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation.LeaveOneOutCrossValidation;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import com.cmclinnovations.data.collections.ObjectPool;

/**
 *
 * @author pb556
 */

public class ReactionInfluenceFilterMarking extends SpeciesFilter {

    protected LPSolver solver;
    protected LPFormat format;
    protected double maxErr = 25;
    protected int numResults = 25;
    //protected double maxCtr = 5;
    protected Map<Species, Reaction> detValidSpecies = new HashMap<Species, Reaction>();
    protected Map<Species, Reaction> detInvalidSpecies = new HashMap<Species, Reaction>();
    protected Map<Species, Reaction> detMarkedSpecies = new HashMap<Species, Reaction>();
    protected Map<Species, Double> markedSpeciesAvgErr = new HashMap<Species, Double>();
    protected Map<Species, Double> validSpeciesAvgErr = new HashMap<Species, Double>();
    protected Map<Species, Double> invalidSpeciesAvgErr = new HashMap<Species, Double>();

    public ReactionInfluenceFilterMarking(LPSolver solver, LPFormat format, double maxErr) {
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

    public Map<Species, Reaction> getDetailedMarkedSpecies() {
        return detMarkedSpecies;
    }

    public Map<Species, Double> getAvgErrorMarkedSpecies() {
        return markedSpeciesAvgErr;
    }

    public Map<Species, Double> getAvgErrorValidSpecies() {
        return validSpeciesAvgErr;
    }

    public Map<Species, Double> getAvgErrorInvalidSpecies() {
        return invalidSpeciesAvgErr;
    }

    protected void identify() {
        boolean execute = true;

        invalidSpecies = new ArrayList<Species>();
        validSpecies = new ArrayList<Species>();

        ArrayList<Species> markedSpecies = new ArrayList<Species>();
        Map<Species, Reaction> res = null;
        Map<Species, Integer> negCtr = null;
        Map<Species, Integer> posCtr = null;

        boolean recalculate = true;

        while (execute) {
            execute = false;

            if (recalculate) {
                LeaveOneOutCrossValidation validation = new LeaveOneOutCrossValidation(
                        getPool(invalidSpecies),
                        new PoolModificationCalculator(numResults, solver, format),
                        new OutlierReactionSelector());
                validation.validate();
                res = validation.getDetailedValidationResults();


                Map<Species, Reaction> acceptableRes = new HashMap<Species, Reaction>();
                Map<Species, Reaction> inacceptableRes = new HashMap<Species, Reaction>();
                for (Species s : res.keySet()) {
                    if (Math.abs(s.getHf() - res.get(s).calculateHf()) > maxErr) {
                        inacceptableRes.put(s, res.get(s));
                        System.out.println(s.getRef() + "(" + Math.abs(s.getHf() - res.get(s).calculateHf()) + "): " + res.get(s).toString());
                    } else {
                        acceptableRes.put(s, res.get(s));
                        if (Math.abs(s.getHf() - res.get(s).calculateHf()) <= 2) {
                            validSpecies.addAll(res.get(s).getProducts().keySet());
                            validSpecies.addAll(res.get(s).getReactants().keySet());
                        }
                    }
                }

                // identification of bad species
                negCtr = count(inacceptableRes);
                posCtr = count(acceptableRes);
                //Map<Species, Integer> posCtr = count(acceptableRes);

            }

            Species maxCtrSpecies = null;
            int maxCtr = -1;

            for (Species s : negCtr.keySet()) {
                boolean containedInvalid = false;
                boolean containedMarked = false;

                for (Species sInvalid : invalidSpecies) {
                    if (s.equals(sInvalid, true)) {
                        containedInvalid = true;
                        break;
                    }
                }

                for (Species sMarked : markedSpecies) {
                    if (s.equals(sMarked, true)) {
                        containedMarked = true;
                        break;
                    }
                }

                if (maxCtrSpecies == null) {
                    maxCtrSpecies = s;
                    maxCtr = negCtr.get(s);
                } else if (maxCtr < negCtr.get(s)
                        && !containedInvalid
                        && !containedMarked) {
                    maxCtrSpecies = s;
                    maxCtr = negCtr.get(s);
                }
            }

            int negCtrCurrent = maxCtr;
            int posCtrCurrent = -1;

            for (Species s : posCtr.keySet()) {
                if (s.equals(maxCtrSpecies, true)) {
                    posCtrCurrent = posCtr.get(s);
                    break;
                }
            }

            recalculate = false;

            if (maxCtrSpecies != null) {
                if (verifyImprovement(maxCtrSpecies, res)) {


                    // check if it can be removed or if it has to be marked
                    //if (verifyObtainedResults(maxCtrSpecies, res)) {

                    boolean containedInvalid = false;

                    for (Species sInvalid : invalidSpecies) {
                        if (maxCtrSpecies.equals(sInvalid, true)) {
                            containedInvalid = true;
                            break;
                        }
                    }
                    if (!containedInvalid) {
                        execute = true;
                        recalculate = true;
                        invalidSpecies.add(maxCtrSpecies);
                        for (Species s : res.keySet()) {
                            if (s.equals(maxCtrSpecies, true)) {
                                detInvalidSpecies.put(maxCtrSpecies, res.get(s));
                                double value = (((double) negCtrCurrent) / ((double) posCtrCurrent));
                                invalidSpeciesAvgErr.put(maxCtrSpecies, (double) negCtrCurrent);
                            }
                        }
                    }
                    //} else {

                    //markedSpecies.add(maxCtrSpecies);
                    //}
                } else {
                    boolean containedMarked = false;

                    for (Species sInvalid : markedSpecies) {
                        if (maxCtrSpecies.equals(sInvalid, true)) {
                            containedMarked = true;
                            break;
                        }
                    }
                    if (!containedMarked) {
                        markedSpecies.add(maxCtrSpecies);
                        double value = (((double) negCtrCurrent) / ((double) posCtrCurrent));
                        markedSpeciesAvgErr.put(maxCtrSpecies, (double) negCtrCurrent);
                        for (Species s : res.keySet()) {
                            if (s.equals(maxCtrSpecies, true)) {
                                detMarkedSpecies.put(maxCtrSpecies, res.get(s));
                            }
                        }
                    }

                    if (negCtr.size() - markedSpecies.size() - invalidSpecies.size() > 0) {
                        execute = true;
                        negCtr.remove(maxCtrSpecies);
                    }
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
                        for (Species s : markedSpecies) {
                            if (k.equals(s, true)) {
                                valid = false;
                                break;
                            }
                        }
                        if (valid) {
                            for (Species s : posCtr.keySet()) {
                                if (s.equals(k, true)) {
                                    posCtrCurrent = posCtr.get(s);
                                    break;
                                }
                            }
                            for (Species s : negCtr.keySet()) {
                                if (s.equals(k, true)) {
                                    negCtrCurrent = negCtr.get(s);
                                    break;
                                }
                            }
                            double value = (((double) negCtrCurrent) / ((double) posCtrCurrent));
                            validSpecies.add(k);
                            detValidSpecies.put(k, res.get(k));
                            validSpeciesAvgErr.put(k, (double) negCtrCurrent);
                        }
                    }
                }
            }
        }
    }

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
