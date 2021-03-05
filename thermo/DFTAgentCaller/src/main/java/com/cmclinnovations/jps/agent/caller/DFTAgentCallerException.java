package com.cmclinnovations.jps.agent.caller;

/**
 * This is the project specific exception class.  
 * 
 * @author msff2
 *
 */
public class DFTAgentCallerException extends Exception{
	public DFTAgentCallerException(){
		super();
	}
	
	public DFTAgentCallerException(String message){
		super(message);
	}
}