package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;
@XmlType(propOrder={"sourceComment", "duplicate", "reversible", "landauTeller", "type", "noncon", "partialpressure", 
		"sitefrac", "id", "comment", "equation", "order", "rateCoeff", "reactants", "products"})
@XmlAccessorType(XmlAccessType.FIELD)
public class Reaction {
	@XmlPath("node[@name='sourceComment']")
	private String sourceComment;
	@XmlAttribute
	private String duplicate;
	@XmlAttribute
	private String reversible;
	@XmlAttribute
	private String landauTeller;
	@XmlAttribute
	private String type;
	@XmlAttribute
	private String noncon;
	@XmlAttribute
	private String partialpressure;
	@XmlAttribute
	private String sitefrac;
	@XmlAttribute
	private String id;
	@XmlPath("node[@name='comment']")
	private String comment;
	@XmlPath("node[@name='equation']")
	private String equation;
	@XmlPath("node[@name='order']")
	private ArrayList<ReactionOrder> order;
	@XmlPath("node[@name='rateCoeff']")
	private RateCoefficient rateCoeff;
	@XmlPath("node[@name='reactants']")
	private String reactants;
	@XmlPath("node[@name='products']")
	private String products;

	public String getSourceComment() {
		return sourceComment;
	}

	public void setSourceComment(String sourceComment) {
		this.sourceComment = sourceComment;
	}

	public String getReversible() {
		return reversible;
	}

	public void setReversible(String reversible) {
		this.reversible = reversible;
	}

	public String getLandauTeller() {
		return landauTeller;
	}

	public void setLandauTeller(String landauTeller) {
		this.landauTeller = landauTeller;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getNoncon() {
		return noncon;
	}

	public void setNoncon(String noncon) {
		this.noncon = noncon;
	}

	public String getPartialpressure() {
		return partialpressure;
	}

	public void setPartialpressure(String partialpressure) {
		this.partialpressure = partialpressure;
	}

	public String getSitefrac() {
		return sitefrac;
	}

	public void setSitefrac(String sitefrac) {
		this.sitefrac = sitefrac;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getEquation() {
		return equation;
	}

	public void setEquation(String equation) {
		this.equation = equation;
	}

	public ArrayList<ReactionOrder> getOrder() {
		return order;
	}

	public void setOrder(ArrayList<ReactionOrder> order) {
		this.order = order;
	}

	public RateCoefficient getRateCoeff() {
		return rateCoeff;
	}

	public void setRateCoeff(RateCoefficient rateCoeff) {
		this.rateCoeff = rateCoeff;
	}

	public String getReactants() {
		return reactants;
	}

	public void setReactants(String reactants) {
		this.reactants = reactants;
	}

	public String getProducts() {
		return products;
	}

	public void setProducts(String products) {
		this.products = products;
	}

	public String getDuplicate() {
		return duplicate;
	}

	public void setDuplicate(String duplicate) {
		this.duplicate = duplicate;
	}
}
