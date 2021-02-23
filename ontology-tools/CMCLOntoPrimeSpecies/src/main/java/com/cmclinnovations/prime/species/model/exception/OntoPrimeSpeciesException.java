package com.cmclinnovations.prime.species.model.exception;

public class OntoPrimeSpeciesException extends Exception {
	/**
	 * Maintains the version number of this class.
	 */
	private static final long serialVersionUID = 882018L;
	
    public OntoPrimeSpeciesException() {
        super();
    }

    public OntoPrimeSpeciesException(String message) {
        super(message);
    }

    public OntoPrimeSpeciesException(String message, Throwable cause) {
        super(message, cause);
    }

    public OntoPrimeSpeciesException(Throwable cause) {
        super(cause);
    }

    protected OntoPrimeSpeciesException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
