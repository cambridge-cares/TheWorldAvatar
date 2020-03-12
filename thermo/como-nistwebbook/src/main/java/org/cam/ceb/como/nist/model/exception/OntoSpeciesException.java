/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.nist.model.exception;

/**
 *
 * @author pb556
 */
public class OntoSpeciesException extends Exception {
	
	private static final long serialVersionUID = 1976L;
	
    public OntoSpeciesException() {
        super();
    }

    public OntoSpeciesException(String message) {
        super(message);
    }

    public OntoSpeciesException(String message, Throwable cause) {
        super(message, cause);
    }

    public OntoSpeciesException(Throwable cause) {
        super(cause);
    }

    protected OntoSpeciesException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
