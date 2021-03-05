package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver;

/**
 * LpSolve exception that is related to IO.
 * 
 * @author pb556
 */
public class LpSolverException extends Exception {

    public LpSolverException(Throwable cause) {
        super(cause);
    }

    public LpSolverException(String message, Throwable cause) {
        super(message, cause);
    }

    public LpSolverException(String message) {
        super(message);
    }

    public LpSolverException() {
        super();
    }

}
