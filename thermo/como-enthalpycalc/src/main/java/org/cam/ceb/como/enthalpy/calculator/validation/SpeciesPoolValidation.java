/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.validation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.MultiRunCalculator;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.PoolModificationCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;

/**
 *
 * @author pb556
 */
public class SpeciesPoolValidation {

    protected ObjectPool<Species> origPool = new ObjectPool<Species>();
    protected ObjectPool<Species> resPool = new ObjectPool<Species>();
    protected Map<Species, Collection<ReactionList>> results = new HashMap<Species, Collection<ReactionList>>();
    protected int numRuns = 1;
    protected int numResults = 1;
    protected int maxDepth = 25;
    protected double maxErrCount = 20.0;
    protected double maxErrPercentage = 20.0;
    protected double maxErrResultsPercentage = 0.1;
    protected int maxErrResultsCount = Integer.MAX_VALUE;
    protected boolean countMode = true;
    protected LPSolver solver = null;
    protected LPFormat format = null;

    public SpeciesPoolValidation(ObjectPool<Species> pool, LPSolver solver, LPFormat format, double maxErr, double maxErrPercentage) {
        countMode = false;
        this.maxErrPercentage = maxErr;
        this.maxErrResultsPercentage = maxErrPercentage;
        origPool = pool;
        this.solver = solver;
        this.format = format;
    }

    public SpeciesPoolValidation(ObjectPool<Species> pool, LPSolver solver, LPFormat format, double maxErr, int maxErrCount) {
        countMode = true;
        this.maxErrCount = maxErr;
        this.maxErrResultsCount = maxErrCount;
        origPool = pool;
        this.solver = solver;
        this.format = format;
    }
    
    public void setMaxSearchDepth(int depth) {
        maxDepth = depth;
    }

    public void setNumberOfRuns(int numRuns) throws Exception {
        if (numRuns <= 0) {
            throw new Exception("Invalid value for the number of runs have been set!");
        }
        this.numRuns = numRuns;
    }

    public void setNumberOfResults(int numResults) throws Exception {
        if (numRuns <= 0) {
            throw new Exception("Invalid value for the number of results have been set!");
        }
        this.numResults = numResults;
    }

    public void setMode(boolean countMode) {
        this.countMode = countMode;
    }

    public void setPercentageModeProperties(double maxErr, double maxErrPercentage) {
        this.maxErrPercentage = maxErr;
        this.maxErrPercentage = maxErrPercentage;
    }

    public void setCountModeProperties(double maxErr, int maxErrCount) {
        this.maxErrCount = maxErr;
        this.maxErrResultsCount = maxErrCount;
    }

    public void validate() throws Exception {
        PoolModificationCalculator poolModCalc = new PoolModificationCalculator(numResults, solver, format);
        poolModCalc.setMaximumSearchDepth(maxDepth);
        MultiRunCalculator c =
                new MultiRunCalculator(
                poolModCalc);
        c.setNumberOfRuns(numRuns);
        results = new HashMap<Species, Collection<ReactionList>>();

        // calculation
        int ctr = 1;
        for (Species s : origPool.getValidatedObjects()) {
            System.out.println("Processing species " + ctr + " / " + origPool.getValidatedObjects().size() + " (" + s.getRef() + ")");
            ctr++;
            c.set(getPool(s, true));
            try {
                c.calculate(s);
                Map<Species, Collection<ReactionList>> r = (Map<Species, Collection<ReactionList>>) c.get();
                for (Species sR : r.keySet()) {
                    results.put(s, r.get(sR));
                }
            } catch (OutOfMemoryError e) {
                System.gc();
            }
        }

        // validation
        if (countMode) {
            validateCountMode();
        } else {
            validatePercentageMode();
        }
    }

    public ObjectPool<Species> getSpeciesPool() {
        return resPool;
    }

    public Map<Species, Collection<ReactionList>> getDetailedResults() {
        return results;
    }

    protected void validateCountMode() {
        Map<Species, Integer> validationRes = validate(maxErrCount);
        resPool = new ObjectPool<Species>();
        for (Species s : validationRes.keySet()) {
            if (validationRes.get(s) <= maxErrResultsCount) {
                resPool.add(s);
            }
        }
        resPool.validateAll();
    }

    protected Map<Species, Integer> validate(double maxErr) {
        Map<Species, Integer> validationRes = new HashMap<Species, Integer>();
        // check and validate results
        for (Species target : results.keySet()) {
            int ctrOff = 0;
            System.out.println("Validating species " + target.getRef());
            for (ReactionList rList : results.get(target)) {
                for (Reaction r : rList) {
                    if (Math.abs(r.getSpecies().getHf() - r.calculateHf()) > maxErr) {
                        ctrOff++;
                    }
                }
            }
            validationRes.put(target, ctrOff);
        }
        return validationRes;
    }

    protected void validatePercentageMode() {
        Map<Species, Integer> validationRes = validate(maxErrPercentage);
        resPool = new ObjectPool<Species>();
        for (Species s : validationRes.keySet()) {
            ReactionList combined = new ReactionList();
            for (ReactionList rList : results.get(s)) {
                combined.addAll(rList);
            }
            double percOff = ((double) validationRes.get(s)) / ((double) combined.size());
            if (percOff <= maxErrResultsPercentage) {
                resPool.add(s);
            }
        }
        resPool.validateAll();
    }

    protected ObjectPool<Species> getPool(Species targetSpecies, boolean validatedSpeciesOnly) {
        Collection<Species> all = origPool.getValidatedObjects();
        if (!validatedSpeciesOnly) {
            all.addAll(origPool.getInvalidatedObjects());
        }
        ObjectPool<Species> newPool = new ObjectPool<Species>();
        for (Species s : all) {
            if (s.equals(targetSpecies, true)) {
                continue;
            }
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }
}
