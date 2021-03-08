/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.singlecore;

import com.cmclinnovations.data.collections.ObjectPool;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.SpeciesFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.Calculator;

/**
 *
 * @author pb556
 * 
 */

public abstract class ObjectPoolCalculator extends Calculator {
    
    protected ObjectPool<Species> pool = null;
    
    public ObjectPoolCalculator() {
        super();
    }
    
    public ObjectPoolCalculator(LPSolver solver, LPFormat format) {
        super(solver, format);
    }
    
    public ObjectPoolCalculator(LPSolver solver, LPFormat format, SpeciesFilter filter) {
        super(solver, format, filter);
    }
    
    public void set(ObjectPool<Species> pool) {
        this.pool = pool;
    }
    
    public ObjectPool<Species> getObjectPool() {
        return pool;
    }
}