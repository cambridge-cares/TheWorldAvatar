package com.cmclinnovations.ontochem.model.exception;

/**
 *
 * @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class TBoxManagementException extends Exception {
	
	private static final long serialVersionUID = 14082018L;
	
    public TBoxManagementException() {
        super();
    }

    public TBoxManagementException(String message) {
        super(message);
    }

    public TBoxManagementException(String message, Throwable cause) {
        super(message, cause);
    }

    public TBoxManagementException(Throwable cause) {
        super(cause);
    }

    protected TBoxManagementException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
