package org.cam.ceb.como.enthalpy.calculator.solver;

/**
 * LpSolve exception when no feasible solution can be found.
 * 
 * @author wp214
 */
public class NoFeasibleSolutionException extends LpSolverException {

    public NoFeasibleSolutionException(Throwable cause) {
        super(cause);
    }

    public NoFeasibleSolutionException(String message, Throwable cause) {
        super(message, cause);
    }

    public NoFeasibleSolutionException(String message) {
        super(message);
    }

    public NoFeasibleSolutionException() {
        super();
    }

}
