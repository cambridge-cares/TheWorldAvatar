package uk.ac.cam.cares.jps.agent.kinetics.simulation;
/**
 * This class is developed to manage exceptions in this project.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class KineticsAgentException extends Exception {
	
	private static final long serialVersionUID = 1976L;
	
    public KineticsAgentException() {
        super();
    }

    public KineticsAgentException(String message) {
        super(message);
    }

    public KineticsAgentException(String message, Throwable cause) {
        super(message, cause);
    }

    public KineticsAgentException(Throwable cause) {
        super(cause);
    }

    protected KineticsAgentException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
