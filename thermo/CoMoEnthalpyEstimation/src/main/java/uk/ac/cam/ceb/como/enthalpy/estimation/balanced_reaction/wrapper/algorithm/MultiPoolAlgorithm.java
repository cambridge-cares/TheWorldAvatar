/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm.validation.SolutionValidation;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.ObjectPoolCalculator;
import com.cmclinnovations.data.collections.ObjectPool;

/**
 *
 * @author pb556
 */
public class MultiPoolAlgorithm extends Algorithm {

    protected List<ObjectPool> pools = new ArrayList<ObjectPool>();
    protected ObjectPoolCalculator calculator;
    protected SolutionValidation validation;
    protected Map<ObjectPool, Map<Species, ReactionList>> calcResults;

    public MultiPoolAlgorithm(ObjectPoolCalculator calculator) {
        this.calculator = calculator;
        this.validation = null;
    }

    public MultiPoolAlgorithm(ObjectPoolCalculator calculator, List<ObjectPool> pools) {
        this.calculator = calculator;
        this.pools = pools;
        this.validation = null;
    }
    
    public MultiPoolAlgorithm(ObjectPoolCalculator calculator, SolutionValidation validation) {
        this.calculator = calculator;
        this.validation = validation;
    }

    public MultiPoolAlgorithm(ObjectPoolCalculator calculator, List<ObjectPool> pools, SolutionValidation validation) {
        this.calculator = calculator;
        this.pools = pools;
        this.validation = validation;
    }

    @Override
    public ReactionList solve(Species targetSpecies) throws Exception {
        // go through pool by pool and apply it to the defined calculator
        if (calculator == null || pools == null || pools.isEmpty()) {
            return new ReactionList();
        }
        ReactionList rList = new ReactionList();
        for (ObjectPool pool : pools) {
            calculator.set(pool);
            calculator.calculate();
            Map<Species, Collection<ReactionList>> d = (Map<Species, Collection<ReactionList>>) calculator.get();
            if (d.size() > 1) {
                throw new InvalidCalculatorSolutionException("Invalid solution obtained!");
            } else {
                if (!calcResults.containsKey(pool)) {
                    calcResults.put(pool, new HashMap<Species, ReactionList>());
                }

                for (Species s : d.keySet()) {
                    if (!calcResults.get(pool).containsKey(s)) {
                        calcResults.get(pool).put(s, new ReactionList());
                    }
                    for (ReactionList list : d.get(s)) {
                        rList.addAll(list);
                        calcResults.get(pool).get(s).addAll(list);
                    }
                }
            }
        }
        return rList;
    }

    @Override
    public void reset() {
        calcResults = new HashMap<ObjectPool, Map<Species, ReactionList>>();
    }

    public Map<Species, ReactionList> get(ObjectPool pool) {
        return calcResults.get(pool);
    }
}
