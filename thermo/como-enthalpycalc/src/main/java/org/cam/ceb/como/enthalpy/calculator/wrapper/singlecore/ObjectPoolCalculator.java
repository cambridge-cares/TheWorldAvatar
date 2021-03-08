/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.singlecore;

import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import org.cam.ceb.como.tools.objectpool.ObjectPool;
import org.cam.ceb.como.enthalpy.calculator.wrapper.Calculator;

/**
 *
 * @author pb556
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
