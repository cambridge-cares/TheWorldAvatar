/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.multiprocessing;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.enthalpy.calculator.solver.SolverHelper;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.PoolModificationCalculator;
import org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore.ObjectPoolCalculator;
import org.cam.ceb.como.tools.objectpool.ObjectPool;

/**
 *
 * @author pb556
 */
public class MPMultiCalculator extends ObjectPoolCalculator {

    protected PoolModificationCalculator calc = null;
    protected int numResults = 15;
    protected ReactionList reactionList = new ReactionList();
    protected MPModel model = null;
    protected int numThreads = 20;

    public MPMultiCalculator() {
        super();
    }

    public MPMultiCalculator(LPSolver solver, LPFormat format, ObjectPool<Species> pool) {
        super(solver, format);
        this.pool = pool;
    }

    public MPMultiCalculator(LPSolver solver, LPFormat format) {
        super(solver, format);
    }

    public MPMultiCalculator(LPSolver solver, LPFormat format, SpeciesFilter filter) {
        super(solver, format, filter);
    }

    public MPMultiCalculator(LPSolver solver, LPFormat format, SpeciesFilter filter, ObjectPool<Species> pool) {
        super(solver, format, filter);
        this.pool = pool;
    }

    public void setNumberOfThreads(int num) {
        numThreads = num;
    }

    public void setNumberOfResults(int num) {
        numResults = num;
    }

    @Override
    public void calculate(Collection<Species> targetSpecies) throws Exception {
        for (Species s : targetSpecies) {
            calculate(s);
        }
    }

    @Override
    public void calculate(Species targetSpecies) throws Exception {
        if (pool != null) {
            boolean identified = false;
            for (Species s : pool.getValidatedObjects()) {
                if (s.equals(s, true)) {
                    pool.invalidate(s);
                    break;
                }
            }
            for (Species s : pool.getInvalidatedObjects()) {
                if (s.equals(s, true)) {
                    identified = true;
                    break;
                }
            }
            if (!identified) {
                pool.addInvalidated(targetSpecies);
            }
        }
        calculate();
    }

    @Override
    public void calculate() throws Exception {
        model = new MPModel();
        MPService service = new MPService(model, numThreads);
        MPManager manager = new MPManager(service);

        for (Species soi : pool.getInvalidatedObjects()) {
            ObjectPool clone = SolverHelper.clone(pool);
            List<Species> species = (List<Species>) clone.getValidatedObjects();
            Collections.shuffle(species);
            System.out.println("Processing species: " + soi.getRef());
            if (!clone.getInvalidatedObjects().isEmpty()) {
                manager.addJob(soi, species, numResults, numResults, solver, format);
            }
        }
        manager.startService();
        while (manager.isProcessing()) {
            try {
                System.out.println("Processing...");
                Thread.sleep(10000);
            } catch (InterruptedException ex) {
                java.util.logging.Logger.getLogger(MPMultiCalculator.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        model.complete();
    }

    @Override
    public Object get() {
        return model;
    }
}
