package org.cam.ceb.como.enthalpy.calculator.solver;

/**
 * LpSolve exception that is related to IO.
 * 
 * @author wp214
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
