package com.cmclinnovations.jps.agent.mechanism.calibration;

public class MoDSAgentException extends Exception{
	public MoDSAgentException() {
		super();
	}
	
	public MoDSAgentException(String message) {
		super(message);
	}
	
	public MoDSAgentException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public MoDSAgentException(Throwable cause) {
		super(cause);
	}
	
	public MoDSAgentException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
