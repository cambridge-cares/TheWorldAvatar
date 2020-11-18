package com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not transport properties have already been parsed. 
 * 
 * @author msff2
 *
 */
public class SpeciesTransportParseStatus {
	boolean transport = false;
	boolean model = false;
	boolean comment = false;
	boolean string = false;
	boolean title = false;
	boolean LJWellDepth = false;
	boolean LJWellDepthUnits = false;
	boolean LJDiameter = false;
	boolean LJDiameterUnits = false;
	boolean dipoleMoment = false;
	boolean dipoleMomentUnits = false;
	boolean polarizability = false;
	boolean polarizabilityUnits = false;
	boolean rotRelax = false;
	boolean rotRelaxUnits = false;
	public boolean isTransport() {
		return transport;
	}
	public void setTransport(boolean transport) {
		this.transport = transport;
	}
	public boolean isModel() {
		return model;
	}
	public void setModel(boolean model) {
		this.model = model;
	}
	public boolean isComment() {
		return comment;
	}
	public void setComment(boolean comment) {
		this.comment = comment;
	}
	public boolean isString() {
		return string;
	}
	public void setString(boolean string) {
		this.string = string;
	}
	public boolean isTitle() {
		return title;
	}
	public void setTitle(boolean title) {
		this.title = title;
	}
	public boolean isLJWellDepth() {
		return LJWellDepth;
	}
	public void setLJWellDepth(boolean lJWellDepth) {
		LJWellDepth = lJWellDepth;
	}
	public boolean isLJWellDepthUnits() {
		return LJWellDepthUnits;
	}
	public void setLJWellDepthUnits(boolean lJWellDepthUnits) {
		LJWellDepthUnits = lJWellDepthUnits;
	}
	public boolean isLJDiameter() {
		return LJDiameter;
	}
	public void setLJDiameter(boolean lJDiameter) {
		LJDiameter = lJDiameter;
	}
	public boolean isLJDiameterUnits() {
		return LJDiameterUnits;
	}
	public void setLJDiameterUnits(boolean lJDiameterUnits) {
		LJDiameterUnits = lJDiameterUnits;
	}
	public boolean isDipoleMoment() {
		return dipoleMoment;
	}
	public void setDipoleMoment(boolean dipoleMoment) {
		this.dipoleMoment = dipoleMoment;
	}
	public boolean isDipoleMomentUnits() {
		return dipoleMomentUnits;
	}
	public void setDipoleMomentUnits(boolean dipoleMomentUnits) {
		this.dipoleMomentUnits = dipoleMomentUnits;
	}
	public boolean isPolarizability() {
		return polarizability;
	}
	public void setPolarizability(boolean polarizability) {
		this.polarizability = polarizability;
	}
	public boolean isPolarizabilityUnits() {
		return polarizabilityUnits;
	}
	public void setPolarizabilityUnits(boolean polarizabilityUnits) {
		this.polarizabilityUnits = polarizabilityUnits;
	}
	public boolean isRotRelax() {
		return rotRelax;
	}
	public void setRotRelax(boolean rotRelax) {
		this.rotRelax = rotRelax;
	}
	public boolean isRotRelaxUnits() {
		return rotRelaxUnits;
	}
	public void setRotRelaxUnits(boolean rotRelaxUnits) {
		this.rotRelaxUnits = rotRelaxUnits;
	}
}
