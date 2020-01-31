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
public class DFTAgentException extends Exception {
	
	private static final long serialVersionUID = 1976L;
	
    public DFTAgentException() {
        super();
    }

    public DFTAgentException(String message) {
        super(message);
    }

    public DFTAgentException(String message, Throwable cause) {
        super(message, cause);
    }

    public DFTAgentException(Throwable cause) {
        super(cause);
    }

    protected DFTAgentException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
