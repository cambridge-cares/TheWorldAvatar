/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.cmclinnovations.ontochem.model.exception;

/**
 *
 * @author pb556
 */
public class OntoException extends Exception {
	
	private static final long serialVersionUID = 1976L;
	
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

    protected OntoException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
