/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.MultiRunCalculator;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.PoolModificationCalculator;
import com.cmclinnovations.data.collections.ObjectPool;

/**
 *
 * @author pb556
 */
public abstract class SpeciesPoolCrossValidation {
    
    protected ObjectPool<Species> origPool = new ObjectPool<Species>();
    protected ObjectPool<Species> resPool = new ObjectPool<Species>();
    protected Map<Species, Collection<ReactionList>> results = new HashMap<Species, Collection<ReactionList>>();
    protected int numRuns = 1;
    protected int numResults = 1;
    protected int maxDepth = 25;
    protected LPSolver solver = null;
    protected LPFormat format = null;

    public SpeciesPoolCrossValidation(ObjectPool<Species> pool, LPSolver solver, LPFormat format) {
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

    public abstract void validate() throws Exception;
    
    public abstract ObjectPool<Species> getReducedSpeciesPool(double maxErr) throws Exception;

    public ObjectPool<Species> getSpeciesPool() {
        return resPool;
    }

    public Map<Species, Collection<ReactionList>> getDetailedResults() {
        return results;
    }

    protected Map<Species, Integer[]> validate(double maxErr) {
        Map<Species, Integer[]> validationRes = new HashMap<Species, Integer[]>();
        // check and validate results
        for (Species target : results.keySet()) {
            int ctrOn = 0;
            int ctrOff = 0;
            System.out.println("Validating species " + target.getRef());
            for (ReactionList rList : results.get(target)) {
                for (Reaction r : rList) {
                    if (Math.abs(r.getSpecies().getHf() - r.calculateHf()) > maxErr) {
                        ctrOff++;
                    } else {
                        ctrOn++;
                    }
                }
            }
            validationRes.put(target, new Integer[] {ctrOn, ctrOff});
        }
        return validationRes;
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
    
    protected ObjectPool<Species> getPool(Collection<Species> species) {
        ObjectPool<Species> newPool = new ObjectPool<Species>();
        for (Species s : species) {
            newPool.add(s);
        }
        newPool.validateAll();
        return newPool;
    }
}
