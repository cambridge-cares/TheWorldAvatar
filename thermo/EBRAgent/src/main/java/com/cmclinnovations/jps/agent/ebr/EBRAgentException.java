/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.cmclinnovations.jps.agent.ebr;

/**
 *
 * @author pb556
 */
public class EBRAgentException extends Exception {
	
	private static final long serialVersionUID = 1976L;
	
    public EBRAgentException() {
        super();
    }

    public EBRAgentException(String message) {
        super(message);
    }

    public EBRAgentException(String message, Throwable cause) {
        super(message, cause);
    }

    public EBRAgentException(Throwable cause) {
        super(cause);
    }

    protected EBRAgentException(String message, Throwable cause,
            boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
