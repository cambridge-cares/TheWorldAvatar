/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore;

import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.Solver;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

/**
 *
 * @author pb556
 */
public class SimpleCalculator extends ObjectPoolCalculator {

    protected HashMap<Species, ReactionList> result = null;

    public SimpleCalculator(LPSolver solver, LPFormat format) {
        super(solver, format);
    }

    public SimpleCalculator(LPSolver solver, LPFormat format, ObjectPool<Species> pool) {
        super(solver, format);
        this.pool = pool;
    }

    public SimpleCalculator(LPSolver solver, LPFormat format, SpeciesFilter filter) {
        super(solver, format, filter);
    }

    public SimpleCalculator(LPSolver solver, LPFormat format, SpeciesFilter filter, ObjectPool<Species> pool) {
        super(solver, format, filter);
        this.pool = pool;
    }

    @Override
    public void calculate(Collection<Species> targetSpecies) throws Exception {
        result = new HashMap<Species, ReactionList>();
        for (Species tSpecies : targetSpecies) {
            Solver enthalpySolver = new Solver(solver, format, new VariableFactory("v"));
            ArrayList<Species> allSpecies = new ArrayList<Species>();
            for (Species s : pool.getValidatedObjects()) {
                //enthalpySolver.addReferenceSpecies(s);
                allSpecies.add(s);
            }
            if (filter != null) {
                allSpecies = (ArrayList<Species>) filter.filter(allSpecies);
            }

            for (Species s : allSpecies) {
                enthalpySolver.addReferenceSpecies(s);
            }

            ReactionList reactionList = new ReactionList();
            Reaction r = enthalpySolver.solve(tSpecies);
            reactionList.add(r);
            result.put(tSpecies, reactionList);
        }
    }

    @Override
    public void calculate(Species targetSpecies) throws Exception {
        HashSet<Species> set = new HashSet<Species>();
        set.add(targetSpecies);
        calculate(set);
    }

    @Override
    public void calculate() throws Exception {
        calculate(pool.getInvalidatedObjects());
    }

    @Override
    public Object get() {
        return result;
    }
}
