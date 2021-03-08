/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.multiprocessing;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPFormat;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.LPSolver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.Solver;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableFactory;
import com.cmclinnovations.data.collections.ObjectPool;

/**
 *
 * @author pb556
 */
public class MPTask {

    protected LPSolver solver;
    protected LPFormat format;
    protected Species targetSpecies;
    protected ObjectPool pool;
    protected Reaction result;
    protected Status status;

    public enum Status {
        SOLVED, UNSOLVABLE, NOT_CALCULATED
    }
    
    public MPTask(LPSolver solver, LPFormat format, Species targetSpecies, ObjectPool pool) {
        this.targetSpecies = targetSpecies;
        this.pool = pool;
        this.format = format;
        this.solver = solver;
        status = Status.NOT_CALCULATED;
    }

    public void setResult(Reaction r, Status status) {
        result = r;
        this.status = status;
    }

    public Status getStatus() {
        return status;
    }
    
    public Reaction getResult() {
        return result;
    }

    public ObjectPool getPool() {
        return pool;
    }

    public Species getTargetSpecies() {
        return targetSpecies;
    }
    
    public LPSolver getLPSolver() {
        return solver;
    }
    
    public LPFormat getLPFormat() {
        return format;
    }
    
    public Solver getSolver() {
        return new Solver(solver, format, new VariableFactory("v"));
    }
}
