package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder= {"atomList", "componentList", "coalList"})
public class ChemicalComposition {
	@XmlElement(name="ns0:atom")
	private ArrayList<Atom> atomList;

	public ArrayList<Atom> getAtom() {
		return atomList;
	}

	public void setAtom(ArrayList<Atom> atomList) {
		this.atomList = atomList;
	}

	@XmlElement(name="ns0:component")
	private ArrayList<Component> componentList;

	public ArrayList<Component> getComponent() {
		return componentList;
	}

	public void setComponent(ArrayList<Component> componentList) {
		this.componentList = componentList;
	}

	@XmlElement
	private ArrayList<Coal> coalList;

	public ArrayList<Coal> getCoal() {
		return coalList;
	}

	public void setCoal(ArrayList<Coal> coalList) {
		this.coalList = coalList;
	}
}
