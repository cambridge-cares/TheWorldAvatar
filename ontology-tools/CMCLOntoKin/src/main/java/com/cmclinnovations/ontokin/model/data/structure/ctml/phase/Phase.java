package com.cmclinnovations.ontokin.model.data.structure.ctml.phase;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import org.eclipse.persistence.oxm.annotations.XmlPath;

@XmlAccessorType(XmlAccessType.FIELD)
public class Phase {

	@XmlPath("node[@name='sourceComment']")
	private String sourceComment;
	
	@XmlAttribute(name="dim")
	private String dimension;
	
	@XmlAttribute
	private String id;
	
	@XmlAttribute
	private String material;
	
	@XmlPath("node[@name='comment']")
	private String comment;
	
	@XmlPath("node[@name='elementArray']")
	private ElementArray elementArray;

	@XmlPath("node[@name='speciesArray']")
	private SpeciesArray speciesArray;
	
	@XmlPath("node[@name='reactionArray']")	
	private ReactionArray reactionArray;
	
	@XmlPath("node[@name='state']")	
	private State state;
	
	@XmlPath("node[@name='thermo']")	
	private Thermo thermo;
	
	@XmlPath("node[@name='kinetics']")	
	private Kinetics kinetics;
	
	@XmlPath("node[@name='transport']")	
	private Transport transport;

	@XmlPath("node[@name='phaseArray']")	
	private String phaseArray;
	
	public String getSourceComment() {
		return sourceComment;
	}

	public void setSourceComment(String sourceComment) {
		this.sourceComment = sourceComment;
	}

	public String getDimension() {
		return dimension;
	}

	public void setDimension(String dimension) {
		this.dimension = dimension;
	}

	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
	}
	
	public String getMaterial() {
		return material;
	}

	public void setMaterial(String material) {
		this.material = material;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public ElementArray getElementArray() {
		return elementArray;
	}

	public void setElementArray(ElementArray elementArray) {
		this.elementArray = elementArray;
	}

	public SpeciesArray getSpeciesArray() {
		return speciesArray;
	}

	public void setSpeciesArray(SpeciesArray speciesArray) {
		this.speciesArray = speciesArray;
	}

	public ReactionArray getReactionArray() {
		return reactionArray;
	}

	public void setReactionArray(ReactionArray reactionArray) {
		this.reactionArray = reactionArray;
	}

	public State getState() {
		return state;
	}

	public void setState(State state) {
		this.state = state;
	}

	public Thermo getThermo() {
		return thermo;
	}

	public void setThermo(Thermo thermo) {
		this.thermo = thermo;
	}

	public Kinetics getKinetics() {
		return kinetics;
	}
	
	public void setKinetics(Kinetics kinetics) {
		this.kinetics = kinetics;
	}

	public Transport getTransport() {
		return transport;
	}
	
	public void setTransport(Transport transport) {
		this.transport = transport;
	}

	public String getPhaseArray() {
		return phaseArray;
	}

	public void setPhaseArray(String phaseArray) {
		this.phaseArray = phaseArray;
	}
}
