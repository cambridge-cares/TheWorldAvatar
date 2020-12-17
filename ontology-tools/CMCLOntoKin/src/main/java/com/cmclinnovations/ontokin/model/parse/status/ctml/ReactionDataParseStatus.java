package com.cmclinnovations.ontokin.model.parse.status.ctml;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not the CTML reactionData element or its attributes have 
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class ReactionDataParseStatus {
	boolean reactionData = false;
	boolean id = false;
	boolean CaseSensitive = false;
	public boolean isReactionData() {
		return reactionData;
	}
	public void setReactionData(boolean reactionData) {
		this.reactionData = reactionData;
	}
	public boolean isId() {
		return id;
	}
	public void setId(boolean id) {
		this.id = id;
	}
	public boolean isCaseSensitive() {
		return CaseSensitive;
	}
	public void setCaseSensitive(boolean caseSensitive) {
		CaseSensitive = caseSensitive;
	}
}
