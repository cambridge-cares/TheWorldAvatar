package com.cmclinnovations.ontokin.model.parse.status.ctml;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not a CTML phase metadata element or attribute has 
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class PhaseParseStatus {
	boolean phase = false;
	boolean phaseDimension = false;
	boolean phaseId = false;
	boolean phaseMaterial = false;
	boolean phaseComment = false;
	public boolean isPhase() {
		return phase;
	}
	public void setPhase(boolean phase) {
		this.phase = phase;
	}
	public boolean isPhaseDimension() {
		return phaseDimension;
	}
	public void setPhaseDimension(boolean phaseDimension) {
		this.phaseDimension = phaseDimension;
	}
	public boolean isPhaseId() {
		return phaseId;
	}
	public void setPhaseId(boolean phaseId) {
		this.phaseId = phaseId;
	}
	public boolean isPhaseMaterial() {
		return phaseMaterial;
	}
	public void setPhaseMaterial(boolean phaseMaterial) {
		this.phaseMaterial = phaseMaterial;
	}
	public boolean isPhaseComment() {
		return phaseComment;
	}
	public void setPhaseComment(boolean phaseComment) {
		this.phaseComment = phaseComment;
	}
}
