package com.cmclinnovations.ontochem.model;

/**
 * Declares the method that initialises all the member variables
 * and class instances required to run CTML to OWL converter. 
 * 
 * @author msff2
 *
 */
public interface IInitCtmlConverter {
	public void init();
	public void initPhase();
	public void initElement();
	public void initSpecies();
	public void initReaction();
	public void initReference();
	public void initReactionOrder();
}
