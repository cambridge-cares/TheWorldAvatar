/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.validation;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
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
public class SpeciesPoolLeaveOneOutCrossValidation extends SpeciesPoolCrossValidation {
    
    public SpeciesPoolLeaveOneOutCrossValidation(ObjectPool<Species> pool, LPSolver solver, LPFormat format) {
        super(pool, solver, format);
    }

    @Override
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
    }

    @Override
    public ObjectPool<Species> getReducedSpeciesPool(double maxErr) throws Exception {
        validate();
        Map<Species, Integer[]> sum = validate(maxErr);
        Collection<Species> valid = new HashSet<Species>();
        for (Species s : sum.keySet()) {
            if (sum.get(s)[0] >= sum.get(s)[1]) {
                valid.add(s);
            }
        }
        return getPool(valid);
    }
}