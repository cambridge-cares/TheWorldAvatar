package uk.ac.cam.cares.jps.base.exception;

public class JPSRuntimeException extends RuntimeException {

	private static final long serialVersionUID = -5203237742932305683L;
	
	public JPSRuntimeException(String message) {
		super(message);
	}

	public JPSRuntimeException(Throwable exc) {
		super(exc);
	}
	
	public JPSRuntimeException(String message, Throwable exc) {
		super(message, exc);
	}
	
}
