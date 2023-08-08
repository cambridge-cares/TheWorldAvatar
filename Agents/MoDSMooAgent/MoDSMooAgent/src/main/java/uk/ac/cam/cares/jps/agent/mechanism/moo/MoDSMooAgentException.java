package uk.ac.cam.cares.jps.agent.mechanism.moo;

public class MoDSMooAgentException extends Exception{
	public MoDSMooAgentException() {
		super();
	}
	
	public MoDSMooAgentException(String message) {
		super(message);
	}
	
	public MoDSMooAgentException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public MoDSMooAgentException(Throwable cause) {
		super(cause);
	}
	
	public MoDSMooAgentException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
