package uk.ac.cam.cares.jps.kg;

public class OntoException extends Exception {
	public OntoException() {
		super();
	}
	
	public OntoException(String message) {
		super(message);
	}
	
	public OntoException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public OntoException(Throwable cause) {
		super(cause);
	}
	
	public OntoException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}
}
