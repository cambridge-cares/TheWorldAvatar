package com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata.reaction;

public class ReactionOrderParseStatus {
	boolean order = false;
	boolean direction = false;
	boolean species = false;
	public boolean isOrder() {
		return order;
	}
	public void setOrder(boolean order) {
		this.order = order;
	}
	public boolean isDirection() {
		return direction;
	}
	public void setDirection(boolean direction) {
		this.direction = direction;
	}
	public boolean isSpecies() {
		return species;
	}
	public void setSpecies(boolean species) {
		this.species = species;
	}
}
