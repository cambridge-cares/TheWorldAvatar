package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver;

/**
 * LpSolve exception when no feasible solution can be found.
 * 
 * @author pb556
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
