package uk.ac.cam.cares.jps.agent.iri.resolution;

/**
 * Defined to manage exceptions of the IRI Resolution Agent.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class IRIResolutionAgentException extends Exception {
	public IRIResolutionAgentException() {
		super();
	}

	public IRIResolutionAgentException(String message) {
		super(message);
	}

	public IRIResolutionAgentException(String message, Throwable cause) {
		super(message, cause);
	}

	public IRIResolutionAgentException(Throwable cause) {
		super(cause);
	}

	protected IRIResolutionAgentException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

}
