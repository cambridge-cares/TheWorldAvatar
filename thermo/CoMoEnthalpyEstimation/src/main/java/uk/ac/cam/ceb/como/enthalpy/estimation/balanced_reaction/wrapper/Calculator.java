/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper;

import java.util.Collection;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.filter.SpeciesFilter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.selector.ReactionSelector;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;

/**
 *
 * @author pb556
 * 
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
