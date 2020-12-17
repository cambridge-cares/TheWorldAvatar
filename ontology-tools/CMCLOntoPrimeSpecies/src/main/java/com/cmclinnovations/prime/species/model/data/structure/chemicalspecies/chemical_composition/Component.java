package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.component.Amount;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.component.SpeciesLink;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.component.Uncertainty;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder= {"speciesLink", "amount", "uncertaintyList"})
public class Component {
	@XmlElement
	private SpeciesLink speciesLink;

	public SpeciesLink getSpeciesLink() {
		return speciesLink;
	}

	public void setSpeciesLink(SpeciesLink speciesLink) {
		this.speciesLink = speciesLink;
	}

	@XmlElement
	private Amount amount;

	public Amount getAmount() {
		return amount;
	}

	public void setAmount(Amount amount) {
		this.amount = amount;
	}

	@XmlElement
	private ArrayList<Uncertainty> uncertaintyList;

	public ArrayList<Uncertainty> getUncertainty() {
		return uncertaintyList;
	}

	public void setUncertainty(ArrayList<Uncertainty> uncertaintyList) {
		this.uncertaintyList = uncertaintyList;
	}
}
