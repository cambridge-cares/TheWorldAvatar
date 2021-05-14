/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm.validation.SolutionValidation;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore.ObjectPoolCalculator;

/**
 *
 * @author pb556
 */
public class MultipleSolverAlgorithm extends Algorithm {

    protected SolutionValidation validation;
    protected Collection<ObjectPoolCalculator> calculators;
    // depends on species and calculator and not only one!!!
    protected Map<ObjectPoolCalculator, Map<Species, ReactionList>> calcResults;

    public MultipleSolverAlgorithm(SolutionValidation validation) {
        calculators = new ArrayList<ObjectPoolCalculator>();
        this.validation = validation;
        calcResults = new HashMap<ObjectPoolCalculator, Map<Species, ReactionList>>();
    }

    public MultipleSolverAlgorithm(Collection<ObjectPoolCalculator> calculators, SolutionValidation validation) {
        this.calculators = calculators;
        this.validation = validation;
        calcResults = new HashMap<ObjectPoolCalculator, Map<Species, ReactionList>>();
    }

    @Override
    public void reset() {
        calcResults = new HashMap<ObjectPoolCalculator, Map<Species, ReactionList>>();
    }

    @Override
    public ReactionList solve(Species targetSpecies) throws Exception {
        if (calculators == null || calculators.isEmpty()) {
            return new ReactionList();
        }
        ReactionList rList = new ReactionList();
        for (ObjectPoolCalculator calc : calculators) {
            calc.calculate(targetSpecies);
            Map<Species, Collection<ReactionList>> d = (Map<Species, Collection<ReactionList>>) calc.get();
            if (d.size() > 1) {
                throw new InvalidCalculatorSolutionException("Invalid solution obtained!");
            } else {
                if (!calcResults.containsKey(calc)) {
                    calcResults.put(calc, new HashMap<Species, ReactionList>());
                }

                for (Species s : d.keySet()) {
                    if (!calcResults.get(calc).containsKey(s)) {
                        calcResults.get(calc).put(s, new ReactionList());
                    }
                    for (ReactionList list : d.get(s)) {
                        rList.addAll(list);
                        calcResults.get(calc).get(s).addAll(list);
                    }
                }
            }
        }
        return rList;
    }

    public ReactionList solveByCalculator(Species targetSpecies) throws Exception {
        if (calculators == null || calculators.isEmpty()) {
            return new ReactionList();
        }
        ReactionList rList = new ReactionList();
        for (ObjectPoolCalculator calc : calculators) {
            try {
                calc.calculate(targetSpecies);
                Map<Species, ReactionList> d = (Map<Species, ReactionList>) calc.get();
                if (d.size() > 1) {
                    throw new InvalidCalculatorSolutionException("Invalid solution obtained!");
                } else {
                    for (Species s : d.keySet()) {
                        rList.addAll(d.get(s));
                    }
                }
            } catch (Exception ex) {
                Logger.getLogger(MultipleSolverAlgorithm.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return rList;
    }

    public Map<Species, ReactionList> get(ObjectPoolCalculator calculator) {
        return calcResults.get(calculator);
    }
}
