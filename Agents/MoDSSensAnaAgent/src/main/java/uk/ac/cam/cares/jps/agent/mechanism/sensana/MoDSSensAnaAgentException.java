package uk.ac.cam.cares.jps.agent.mechanism.sensana;

public class MoDSSensAnaAgentException extends Exception{
	public MoDSSensAnaAgentException() {
		super();
	}
	
	public MoDSSensAnaAgentException(String message) {
		super(message);
	}
	
	public MoDSSensAnaAgentException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public MoDSSensAnaAgentException(Throwable cause) {
		super(cause);
	}
	
	public MoDSSensAnaAgentException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
