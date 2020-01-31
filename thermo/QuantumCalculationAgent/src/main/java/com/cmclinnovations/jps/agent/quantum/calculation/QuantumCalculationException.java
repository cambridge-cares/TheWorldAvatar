/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.cmclinnovations.jps.agent.quantum.calculation;

/**
 *
 * @author pb556
 */
public class QuantumCalculationException extends Exception {
	
	private static final long serialVersionUID = 1976L;
	
    public QuantumCalculationException() {
        super();
    }

    public QuantumCalculationException(String message) {
        super(message);
    }

    public QuantumCalculationException(String message, Throwable cause) {
        super(message, cause);
    }

    public QuantumCalculationException(Throwable cause) {
        super(cause);
    }

    protected QuantumCalculationException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
