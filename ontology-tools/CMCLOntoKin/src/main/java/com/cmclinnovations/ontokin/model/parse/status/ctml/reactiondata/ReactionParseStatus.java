package com.cmclinnovations.ontokin.model.parse.status.ctml.reactiondata;

public class ReactionParseStatus {
	boolean reaction = false;
	boolean duplicate = false;
	boolean reversible = false;
	boolean landauTeller = false;
	boolean type = false;
	boolean nonCon = false;
	boolean partialPressure = false;
	boolean siteFrac = false;
	boolean id = false;
	boolean comment = false;
	boolean equation = false;
	boolean reactants = false;
	boolean products = false;
	public boolean isReaction() {
		return reaction;
	}
	public void setReaction(boolean reaction) {
		this.reaction = reaction;
	}
	public boolean isDuplicate() {
		return duplicate;
	}
	public void setDuplicate(boolean duplicate) {
		this.duplicate = duplicate;
	}
	public boolean isReversible() {
		return reversible;
	}
	public void setReversible(boolean reversible) {
		this.reversible = reversible;
	}
	public boolean isLandauTeller() {
		return landauTeller;
	}
	public void setLandauTeller(boolean landauTeller) {
		this.landauTeller = landauTeller;
	}
	public boolean isType() {
		return type;
	}
	public void setType(boolean type) {
		this.type = type;
	}
	public boolean isNonCon() {
		return nonCon;
	}
	public void setNonCon(boolean nonCon) {
		this.nonCon = nonCon;
	}
	public boolean isPartialPressure() {
		return partialPressure;
	}
	public void setPartialPressure(boolean partialPressure) {
		this.partialPressure = partialPressure;
	}
	public boolean isSiteFrac() {
		return siteFrac;
	}
	public void setSiteFrac(boolean siteFrac) {
		this.siteFrac = siteFrac;
	}
	public boolean isId() {
		return id;
	}
	public void setId(boolean id) {
		this.id = id;
	}
	public boolean isComment() {
		return comment;
	}
	public void setComment(boolean comment) {
		this.comment = comment;
	}
	public boolean isEquation() {
		return equation;
	}
	public void setEquation(boolean equation) {
		this.equation = equation;
	}
	public boolean isReactants() {
		return reactants;
	}
	public void setReactants(boolean reactants) {
		this.reactants = reactants;
	}
	public boolean isProducts() {
		return products;
	}
	public void setProducts(boolean products) {
		this.products = products;
	}
}
