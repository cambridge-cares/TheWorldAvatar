package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not the reaction array tag of a phase and its attributes have
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class ReactionArrayParseStatus {
	boolean reactionArray = false;
	boolean reactionDataSrc = false;
	
	public boolean isReactionArray() {
		return reactionArray;
	}
	public void setReactionArray(boolean reactionArray) {
		this.reactionArray = reactionArray;
	}
	public boolean isReactionDataSrc() {
		return reactionDataSrc;
	}
	public void setReactionDataSrc(boolean reactionDataSrc) {
		this.reactionDataSrc = reactionDataSrc;
	}
}
