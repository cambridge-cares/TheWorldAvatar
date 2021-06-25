package uk.ac.cam.cares.jps.agent.mechanism.datadriven;

public class MoDSDataDrivenAgentException extends Exception{
	public MoDSDataDrivenAgentException() {
		super();
	}
	
	public MoDSDataDrivenAgentException(String message) {
		super(message);
	}
	
	public MoDSDataDrivenAgentException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public MoDSDataDrivenAgentException(Throwable cause) {
		super(cause);
	}
	
	public MoDSDataDrivenAgentException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
