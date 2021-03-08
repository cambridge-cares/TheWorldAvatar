/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.validation;

import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.ObjectPoolCalculator;
import org.cam.ceb.como.math.sampler.Sampler;
import org.cam.ceb.como.math.statistics.validation.CrossValidation;
import org.cam.ceb.como.math.statistics.validation.CrossValidationResult;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class KFoldCrossValidation extends CrossValidation {

    protected int k = 0;
    protected Sampler sampler = null;
    protected ObjectPoolCalculator calculator = null;
    protected Collection<CrossValidationResult> results = new HashSet<CrossValidationResult>();
    private Logger logger = Logger.getLogger(getClass());

    public KFoldCrossValidation(int k, ObjectPool<Species> pool, Sampler sampler, ObjectPoolCalculator calculator) {
        super(pool);
        this.k = k;
        this.sampler = sampler;
        this.calculator = calculator;
    }

    @Override
    public void validate() {
        // run the k fold cross validation
        Map<Integer, ObjectPool> samples = createSubSets();
        results = new HashSet<CrossValidationResult>();
        for (Integer i : samples.keySet()) {
            CrossValidationResult res = new CrossValidationResult();
            ObjectPool<Species> ref = new ObjectPool<Species>();
            for (Integer j : samples.keySet()) {
                if (i != j) {
                    ref.addAll(samples.get(j).getValidatedObjects());
                }
            }
            res.set(ref);
            Collection<Species> targetSpecies = samples.get(i).getValidatedObjects();
            calculator.set(ref);
            try {
                //Map<Species, ReactionList> sol = calculator.calculate(targetSpecies);                
//                Map<Species, Reaction> kSol = new HashMap<Species, Reaction>();
//                // calculate the validation data for the calculation
//                for (Species s : sol.keySet()) {
//                    kSol.put(s, getReaction(sol.get(s)));
//                }
//                res.set(kSol);
                //res.set(sol);
                //results.add(res);
            } catch (Exception ex) {
                logger.error(ex.getMessage(), ex);
            }
        }
    }

    @Override
    public Collection<CrossValidationResult> getValidationResults() {
        return results;
    }

    // create the k samples
    // what algorithm for the subsampling was chosen
    protected Map<Integer, ObjectPool> createSubSets() {
        pool.validateAll();
        HashMap<Integer, ObjectPool> samples = new HashMap<Integer, ObjectPool>();
        ArrayList<ArrayList> subsets = (ArrayList<ArrayList>) sampler.randomSample((ArrayList) pool.getValidatedObjects(), k);
        for (int i = 0; i < subsets.size(); i++) {
            ObjectPool samplePool = new ObjectPool();
            samplePool.addAll(subsets.get(i));
            samplePool.validateAll();
            samples.put(i + 1, samplePool);
        }
        return samples;
    }

    protected Reaction getReaction(ReactionList list) {
        double[] val = new double[list.size()];
        Reaction[] r = new Reaction[list.size()];
        for (int i = 0; i < list.size(); i++) {
            val[i] = list.get(i).calculateHf();
            r[i] = list.get(i);
        }
        // sort it
        for (int i = 0; i < val.length; i++) {
            for (int j = i + 1; j < val.length; j++) {
                if (val[i] > val[j]) {
                    // swap
                    double m = val[i];
                    Reaction rM = r[i];
                    val[i] = val[j];
                    r[i] = r[j];
                    val[j] = m;
                    r[j] = rM;
                }
            }
        }
        
//        for (int i = 0; i < r.length; i++) {
//            System.out.println(r[i].calculateHf());
//        }
        // get median (sort off)
        int index = (int) (r.length / 2.0);
        return r[index];
    }
}
