package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_identifier;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;

@XmlType(propOrder={"nameList"})
@XmlAccessorType(XmlAccessType.FIELD)
public class ChemicalIdentifier {
	@XmlElement(name="ns0:name")
	private ArrayList<Name> nameList;

	public ArrayList<Name> getName() {
		return nameList;
	}

	public void setName(ArrayList<Name> nameList) {
		this.nameList = nameList;
	}
}
