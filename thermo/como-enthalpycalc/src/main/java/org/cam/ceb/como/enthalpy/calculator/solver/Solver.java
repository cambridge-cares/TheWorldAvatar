/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableSet;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.Variable;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;

/**
 *
 * @author pb556
 */
public class Solver {

    private Logger logger = Logger.getLogger(getClass());
    private Set<Species> speciesSet;
    private LPSolver lpSolver;
    private VariableFactory vfactory;
    private VariableSet vSet;
    private LPFormat fFormat;

    public Solver(LPSolver solver, LPFormat format, VariableFactory factory) {
        lpSolver = solver;
        speciesSet = new HashSet<Species>();
        fFormat = format;
        vfactory = factory;
        vSet = new VariableSet(vfactory);
    }

    /**
     * Add reference species (as well as their enthalpy of formation, electronic
     * energy + (H(T)-H(0)) + ZPE at the same temperature T for all species).
     *
     * @param species add reference species
     */
    public void addReferenceSpecies(Species species) {
        speciesSet.add(species);
    }
    
    public void removeReferenceSpecies(Species species) {
        speciesSet.remove(species);
    }

    private Map<String, Number> solveProblem(Species targetSpecies) throws NoFeasibleSolutionException, LpSolverException {
        vSet.getVariableOf(targetSpecies);
        for (Species s : speciesSet) {
            vSet.getVariableOf(s);
        }
        // create equation file
        String lpInputString = buildLpSolveInputString(targetSpecies);
        //logger.debug(lpInputString);
        try {
            // call lp_solve input
            Map<String, Number> solutions = lpSolver.solve(lpInputString);
            Set<String> absNamesToBeRemoved = new HashSet<String>();
            // speciesToVariables should now be populated
            for (Map.Entry<String, Number> entry : solutions.entrySet()) {
                String varName = entry.getKey();
                Species sp = vSet.findSpeciesByVariableName(varName);
                Variable variable = vSet.getVariableOf(sp);
                absNamesToBeRemoved.add(variable.absName);
            }
            for (String absName : absNamesToBeRemoved) {
                solutions.remove(absName);
            }
            return solutions;
        } catch (NoFeasibleSolutionException ex) {
            throw ex;
        } catch (LpSolverException ex) {
            throw ex;
        }
    }

    /**
     * Solve isodemic reaction problem for the targetSpecies. All energies
     * (except enthalpy of formation must be provided at the same temperature as
     * those of the reference species).
     *
     * @param targetSpecies target species whose enthalpy of formation needs to
     * be calculated
     * @return An isodesmic reaction. The enthalpy of formation can be evaluated
     * using its method.
     * @throws NoFeasibleSolutionException if there is not solution to the
     * problem.
     * @throws LpSolverException if there is a porblem with IO
     */
    public Reaction solve(Species targetSpecies) throws NoFeasibleSolutionException, LpSolverException {
        vfactory.reset();
        vSet = new VariableSet(vfactory);
        
//        for (Species ref : speciesSet) {
//            if (ref.equals(targetSpecies, true)) {
//                Reaction reaction = new Reaction(targetSpecies);
//                reaction.addReactant(targetSpecies, 1);
//                reaction.addProduct(ref, 1);
//                return reaction;
//            }
//        }
        
        Map<String, Number> solutions = solveProblem(targetSpecies);
        Reaction reaction = new Reaction(targetSpecies);
        for (Map.Entry<String, Number> entry : solutions.entrySet()) {
            String varName = entry.getKey();
            Species sp = vSet.findSpeciesByVariableName(varName);
            if (sp == null) {
                throw new RuntimeException("Expecting a species for variable name " + varName + " but it was not found. Possible cause is that "
                        + getClass().getSimpleName() + " was modified externally before the solver finished. This solver is not thread-safe");
            }
            Double count = (Double) entry.getValue().doubleValue();
            if (count > 0) {
                reaction.addReactant(sp, count);
            } else if (count < 0) {
                reaction.addProduct(sp, -count);
            } else if (count == 0) {
                // do nothing
            } else {
                throw new RuntimeException("Unexpected count value for species. This should not happen.");
            }
        }
        return reaction;
    }

    private String buildLpSolveInputString(Species targetSpecies) {
        return fFormat.getInputString(targetSpecies, speciesSet, vSet);
    }
}
