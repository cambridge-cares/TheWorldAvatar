package uk.ac.cam.cares.jps.agent.mechanism.coordination;

public class AutoMechCalibAgentException extends Exception{
	public AutoMechCalibAgentException() {
		super();
	}
	
	public AutoMechCalibAgentException(String message) {
		super(message);
	}
	
	public AutoMechCalibAgentException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public AutoMechCalibAgentException(Throwable cause) {
		super(cause);
	}
	
	public AutoMechCalibAgentException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
