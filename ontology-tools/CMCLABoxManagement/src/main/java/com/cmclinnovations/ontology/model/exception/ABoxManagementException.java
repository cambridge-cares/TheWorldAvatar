/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.cmclinnovations.ontology.model.exception;

/**
 * This is a class created to manage catching and throwing</br>
 * exception related to the data representation in ABox using OWL.    
 *
 * @author Feroz Farazi (msff2@cam.ac.uk)
 */
public class ABoxManagementException extends Exception {
	
	/**
	 * Maintains the version number of this class.
	 */
	private static final long serialVersionUID = 1082018L;
	
    public ABoxManagementException() {
        super();
    }

    public ABoxManagementException(String message) {
        super(message);
    }

    public ABoxManagementException(String message, Throwable cause) {
        super(message, cause);
    }

    public ABoxManagementException(Throwable cause) {
        super(cause);
    }

    protected ABoxManagementException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
