/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.cmclinnovations.ontochemexp.model.exception;

/**
 * This is a class created to manage catching and throwing</br>
 * exception related to the OntoChemExp data representation using OWL.    
 *
 * @author msff2@cam.ac.uk
 */
public class OntoChemExpException extends Exception {
	
	/**
	 * Maintains the version number of this class.
	 */
	private static final long serialVersionUID = 882018L;
	
    public OntoChemExpException() {
        super();
    }

    public OntoChemExpException(String message) {
        super(message);
    }

    public OntoChemExpException(String message, Throwable cause) {
        super(message, cause);
    }

    public OntoChemExpException(Throwable cause) {
        super(cause);
    }

    protected OntoChemExpException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
