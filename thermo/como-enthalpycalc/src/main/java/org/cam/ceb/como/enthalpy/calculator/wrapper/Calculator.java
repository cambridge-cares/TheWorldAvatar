/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper;

import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.filter.SpeciesFilter;
import org.cam.ceb.como.enthalpy.calculator.solver.LPFormat;
import org.cam.ceb.como.enthalpy.calculator.solver.LPSolver;
import java.util.Collection;
import org.cam.ceb.como.enthalpy.calculator.reaction.selector.ReactionSelector;

/**
 *
 * @author pb556
 */
public abstract class Calculator {

    protected LPSolver solver = null;
    protected LPFormat format = null;
    protected SpeciesFilter filter = null;
    protected ReactionSelector selector = null;

    public Calculator() {
        
    }
    
    public Calculator(LPSolver solver, LPFormat format) {
        this.solver = solver;
        this.format = format;
    }
    
//    public Calculator(LPSolver solver, LPFormat format, ReactionSelector selector) {
//        this.solver = solver;
//        this.format = format;
//        this.selector = selector;
//    }

    public Calculator(LPSolver solver, LPFormat format, SpeciesFilter filter) {
        this.solver = solver;
        this.format = format;
        this.filter = filter;
    }
    
//    public Calculator(LPSolver solver, LPFormat format, SpeciesFilter filter, ReactionSelector selector) {
//        this.solver = solver;
//        this.format = format;
//        this.filter = filter;
//        this.selector = selector;
//    }

    public void setSpeciesFilter(SpeciesFilter filter) {
        this.filter = filter;
    }

    public void setReactionSelector(ReactionSelector selector) {
        this.selector = selector;
    }
    
    public void setSolver(LPSolver solver) {
        this.solver = solver;
    }

    public void setFormat(LPFormat format) {
        this.format = format;
    }
    
    public LPSolver getSolver() {
        return solver;
    }
    
    public LPFormat getFormat() {
        return format;
    }
    
    public SpeciesFilter getSpeciesFilter() {
        return filter;
    }
    
    public ReactionSelector getReactionSelector() {
        return selector;
    }

    public abstract void calculate(Collection<Species> targetSpecies) throws Exception;

    public abstract void calculate(Species targetSpecies) throws Exception;

    public abstract void calculate() throws Exception;

    public abstract Object get();
    
    //public abstract void reset();
}
