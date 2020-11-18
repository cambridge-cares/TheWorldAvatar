package com.cmclinnovations.ontokin.model.data.structure.ctml;

import java.util.ArrayList;

import com.cmclinnovations.ontokin.model.data.structure.ctml.element.ElementData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.Phase;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.ReactionData;
import com.cmclinnovations.ontokin.model.data.structure.ctml.species.SpeciesData;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Holds the skeleton of CTML containing its macro components, e.g.,
 * phases, elements, species and reactions.
 * 
 * @author msff2
 *
 */
@XmlRootElement
@XmlType(propOrder={"comment", "validate", "phase", "elementData", "speciesData", "reactionData"})
@XmlAccessorType(XmlAccessType.FIELD)
public class Ctml {

	@XmlAttribute(name="cmcl_version")
	private String cmcl_version;

	@XmlAttribute
	private String commit;

	@XmlElement
	private ArrayList<CtmlComment> comment;

	@XmlElement
	private Validate validate;

	@XmlElement
	private ArrayList<Phase> phase;

	@XmlElement
	private ElementData elementData;
	
	@XmlElement
	private ArrayList<SpeciesData> speciesData;
	
	@XmlElement
	private ArrayList<ReactionData> reactionData;

	public String getCmclVersion() {
		return cmcl_version;
	}

	public void setCmclVersion(String cmcl_version) {
		this.cmcl_version = cmcl_version;
	}

	public String getCommit() {
		return commit;
	}
	
	public void setCommit(String commit) {
		this.commit = commit;
	}

	public String getCmcl_version() {
		return cmcl_version;
	}

	public void setCmcl_version(String cmcl_version) {
		this.cmcl_version = cmcl_version;
	}

	public ArrayList<CtmlComment> getComment() {
		return comment;
	}

	public void setComment(ArrayList<CtmlComment> comment) {
		this.comment = comment;
	}

	public Validate getValidate() {
		return validate;
	}

	public void setValidate(Validate validate) {
		this.validate = validate;
	}

	public ArrayList<Phase> getPhase() {
		return phase;
	}

	public void setPhase(ArrayList<Phase> phase) {
		this.phase = phase;
	}

	public ElementData getElementData() {
		return elementData;
	}

	public void setElementData(ElementData elementData) {
		this.elementData = elementData;
	}

	public ArrayList<SpeciesData> getSpeciesData() {
		return speciesData;
	}

	public void setSpeciesData(ArrayList<SpeciesData> speciesData) {
		this.speciesData = speciesData;
	}

	public ArrayList<ReactionData> getReactionData() {
		return reactionData;
	}

	public void setReactionData(ArrayList<ReactionData> reactionData) {
		this.reactionData = reactionData;
	}
}
