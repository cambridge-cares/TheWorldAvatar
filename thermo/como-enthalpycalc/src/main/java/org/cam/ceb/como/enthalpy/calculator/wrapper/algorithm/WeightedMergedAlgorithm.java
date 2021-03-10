/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.algorithm;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.ObjectPoolCalculator;

/**
 *
 * @author pb556
 */
public class WeightedMergedAlgorithm extends Algorithm {

    private Logger logger = Logger.getLogger(getClass());
    protected Map<ReactionList, Double> wRes = null;
    protected Map<ObjectPoolCalculator, ReactionList> res = null;
    protected Map<ObjectPoolCalculator, Double> calculators = null;
    protected int num = 1;

    // add some additional methods
    // add, remove
    // get all results
    // First-In-First-Out algorithm in regards of calculators
    public WeightedMergedAlgorithm(Map<ObjectPoolCalculator, Double> calculators, int num) {
        this.calculators = calculators;
        this.num = num;
        res = new HashMap<ObjectPoolCalculator, ReactionList>();
        wRes = new HashMap<ReactionList, Double>();
    }

    @Override
    public ReactionList solve(Species targetSpecies) throws Exception {
        if (calculators == null || num <= 0) {
            return null;
        }
        
        
//        
//        List<ObjectPoolCalculator> list = calculators.getOrderedListOfCalculators();
//        for (int i = 0; i < list.size(); i++) {
//            try {
//                list.get(i).calculate(targetSpecies);
//                successfulCalculator.put(targetSpecies, list.get(i));
//                Map<Species, ReactionList> res = (Map<Species, ReactionList>) list.get(i).get();
//                
//                // add results to result object from the parent
//                Species key = null;
//                for (Species s : results.keySet()) {
//                    if (s.equals(targetSpecies, true)) {
//                        key = s;
//                        break;
//                    }
//                }
//                
//                for (Species s : res.keySet()) {
//                    if (s.equals(targetSpecies, true)) {
//                        if (key != null) {
//                            ReactionList rList = results.get(key);
//                            rList.addAll(res.get(s));
//                            results.put(key, rList);
//                        } else {
//                            results.put(targetSpecies, res.get(s));
//                            key = targetSpecies;
//                        }
//                        return results.get(key);
//                    }
//                }
//            } catch (Exception e) {
//                logger.trace("Calculator " + (i) + " did not return a result for species " + targetSpecies.getRef() + "!");
//                continue;
//            }
//        }
        
        return new ReactionList();
    }

    @Override
    public void reset() {
        results = new HashMap<Species, ReactionList>();
        res = new HashMap<ObjectPoolCalculator, ReactionList>();
        wRes = new HashMap<ReactionList, Double>();
    }
    // special selection required
}
