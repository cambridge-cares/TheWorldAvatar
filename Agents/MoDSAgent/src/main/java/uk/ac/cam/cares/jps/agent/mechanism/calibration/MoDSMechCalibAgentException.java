package uk.ac.cam.cares.jps.agent.mechanism.calibration;

public class MoDSMechCalibAgentException extends Exception{
	public MoDSMechCalibAgentException() {
		super();
	}
	
	public MoDSMechCalibAgentException(String message) {
		super(message);
	}
	
	public MoDSMechCalibAgentException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public MoDSMechCalibAgentException(Throwable cause) {
		super(cause);
	}
	
	public MoDSMechCalibAgentException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
