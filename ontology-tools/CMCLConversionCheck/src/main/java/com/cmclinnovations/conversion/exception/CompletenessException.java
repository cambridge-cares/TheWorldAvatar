/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.cmclinnovations.conversion.exception;

/**
 * This is a class created to manage catching and throwing exception</br>
 * while checking the difference between a source file and a target file.     
 *
 * @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class CompletenessException extends Exception {
	
	/**
	 * Maintains the version number of this class.
	 */
	private static final long serialVersionUID = 1082018L;
	
    public CompletenessException() {
        super();
    }

    public CompletenessException(String message) {
        super(message);
    }

    public CompletenessException(String message, Throwable cause) {
        super(message, cause);
    }

    public CompletenessException(Throwable cause) {
        super(cause);
    }

    protected CompletenessException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
