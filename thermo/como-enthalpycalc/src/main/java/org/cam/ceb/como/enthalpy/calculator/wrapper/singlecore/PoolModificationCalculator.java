/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import org.cam.ceb.como.combination.Combination;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.LpSolverException;
import org.cam.ceb.como.enthalpy.calculator.solver.Solver;
import org.cam.ceb.como.enthalpy.calculator.solver.SolverHelper;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableFactory;
import org.cam.ceb.como.tools.objectpool.ObjectPool;

/**
 *
 * @author pb556
 * 
 */
public class PoolModificationCalculator extends ObjectPoolCalculator {

    protected HashMap<Species, ReactionList> result = null;
    protected PoolModificationCalculator calc = null;
    protected int numResults = 25;
    protected int maxDepth = 100;
    protected int maxSearches = Integer.MAX_VALUE;
    protected ReactionList reactionList = new ReactionList();
    protected int numSearches = 0;

    public PoolModificationCalculator(int numResults) {
    	
        super();
        this.numResults = numResults;
        
    }

    public PoolModificationCalculator(int numResults, LPSolver solver, LPFormat format, ObjectPool<Species> pool) {
        super(solver, format);
        this.pool = pool;
        result = new HashMap<Species, ReactionList>();
        this.numResults = numResults;
    }

    public PoolModificationCalculator(int numResults, LPSolver solver, LPFormat format) {
        super(solver, format);
        result = new HashMap<Species, ReactionList>();
        this.numResults = numResults;
    }

    public PoolModificationCalculator(int numResults, LPSolver solver, LPFormat format, SpeciesFilter filter) {
        super(solver, format, filter);
        result = new HashMap<Species, ReactionList>();
        this.numResults = numResults;
    }

    public PoolModificationCalculator(int numResults, LPSolver solver, LPFormat format, SpeciesFilter filter, ObjectPool<Species> pool) {
        super(solver, format, filter);
        this.pool = pool;
        result = new HashMap<Species, ReactionList>();
        this.numResults = numResults;
    }

    public int getMaximumNumberOfResults() {
        return numResults;
    }

    public int getMaximumSearchDepth() {
        return maxDepth;
    }

    public int getMaximumNumberOfSearchPools() {
        return maxSearches;
    }

    public void setProperties(int numResults, int depth, int searches) {
        setMaximumNumberOfResults(numResults);
        setMaximumNumberOfSearchPools(searches);
        setMaximumSearchDepth(depth);
    }

    public void setMaximumNumberOfResults(int numResults) {
        if (numResults > 0) {
            this.numResults = numResults;
        } else {
            // logger warning!
        }
    }

    public void setMaximumSearchDepth(int depth) {
        if (depth > 0) {
            this.maxDepth = depth;
        } else {
            // logger warning!
        }
    }

    public void setMaximumNumberOfSearchPools(int searches) {
        if (searches > 0) {
            this.maxSearches = searches;
        } else {
            // logger warning!
        }
    }

    @Override
    public void calculate(Collection<Species> targetSpecies) throws Exception {
        HashMap<Species, ReactionList> sol = new HashMap<Species, ReactionList>();
        for (Species s : targetSpecies) {
            reactionList = new ReactionList();
            numSearches = 0;
            calculate(0, s, pool);
            sol.put(s, reactionList);
        }
        result = sol;
    }

    @Override
    public void calculate(Species targetSpecies) throws Exception {
        HashSet<Species> set = new HashSet<Species>();
        set.add(targetSpecies);
        //System.out.println("Calculate " + targetSpecies.getRef());
        calculate(set);
        result.put(targetSpecies, reactionList);
    }

    protected void calculate(int depth, Species targetSpecies, ObjectPool<Species> refPool) throws LpSolverException {
        numSearches++;
        Solver enthalpySolver = new Solver(solver, format, new VariableFactory("v"));
        ObjectPool p = SolverHelper.clone(refPool);
        List<Species> species = (List<Species>) p.getValidatedObjects();
        Collections.shuffle(species);
        for (Species s : species) {
            enthalpySolver.addReferenceSpecies(s);
        }

        try {
            Reaction r = enthalpySolver.solve(targetSpecies);
            if (!reactionList.has(r)) {
                reactionList.add(r);
            }
            if (reactionList.size() >= numResults || depth >= maxDepth || numSearches > maxSearches) {
                return;
            }
            ArrayList list = new ArrayList<Species>();
            for (Species s : r.getProducts().keySet()) {
                if (!s.equals(targetSpecies, false)) {
                    list.add(s);
                }
                //p.invalidate(s);
            }
            for (Species s : r.getReactants().keySet()) {
                if (!s.equals(targetSpecies, false)) {
                    list.add(s);
                }
                //p.invalidate(s);
            }
            //List combs = any(list.size(), list);
            List combs = Combination.compute(list);
            for (int i = 0; i < combs.size(); i++) {
                List<Species> l = (List<Species>) combs.get(i);
                for (Species s : l) {
                    p.invalidate(s);
                }
                try {
                    calculate(depth++, targetSpecies, p);
                } catch (OutOfMemoryError oome) {
                    System.gc();
                    return;
                } catch (Exception e) {
                    for (int j = i + 1; j < combs.size(); j++) {
                        List<Species> lCheck = (List<Species>) combs.get(i);
                        boolean identified = true;
                        for (Species s : l) {
                            if (!lCheck.contains(s)) {
                                identified = false;
                                break;
                            }
                        }
                        if (identified) {
                            combs.remove(j);
                            j--;
                        }
                    }
                }
                for (Species s : l) {
                    p.validate(s);
                }
                if (reactionList.size() >= numResults) {
                    return;
                }
            }
        } catch (Exception e) {
        }
    }

    // returns multiple possible reactions for the same target species
    public List<List> any(final int n, final List list) {
        if (n < 0) {
            throw new RuntimeException("Choose Any less than zero : n = " + n);
        }
        if (n > list.size()) {
            throw new RuntimeException("Choose Any larger than the size of the list : n = " + n);
        }
        List<List> all = new ArrayList<List>();
        if (n == 0) {
            List sublist = new ArrayList();
            all.add(sublist);
            return all;
        } else if (n == 1) {
            for (Object l : list) {
                List sublist = new ArrayList();
                sublist.add(l);
                all.add(sublist);
            }
            return all;
        } else {
            for (int i = 0; i < list.size(); i++) {
                List rest = new ArrayList(list);
                Object left = list.get(i);
                List<List> rlist = any(n - 1, rest);
                for (List l : rlist) {
                    l.add(0, left);
                    all.add(l);
                }
            }
            return all;
        }
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
