package uk.ac.cam.cares.jps.agent.gPROMS;
/**
 * This class is developed to manage exceptions in this project.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class gPROMSAgentException extends Exception {
	
	private static final long serialVersionUID = 1976L;
	
    public gPROMSAgentException() {
        super();
    }

    public gPROMSAgentException(String message) {
        super(message);
    }

    public gPROMSAgentException(String message, Throwable cause) {
        super(message, cause);
    }

    public gPROMSAgentException(Throwable cause) {
        super(cause);
    }

    protected gPROMSAgentException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
