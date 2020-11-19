package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction;

import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.RateCoeffFloatArray;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.PMin;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMax;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb.TMin;

@XmlType(propOrder={"Arrhenius", "landauTeller", "Tmin", "Tmax", "Pmin", "Pmax", "floatArray", "falloff", "efficiencies"})
@XmlAccessorType(XmlAccessType.FIELD)
public class RateCoefficient {
	@XmlElement
	private List<Arrhenius> Arrhenius;
	@XmlElement
	private LandauTeller landauTeller;
	@XmlElement
	private FallOff falloff;
	@XmlElement
	private Efficiencies efficiencies;
	@XmlElement
	private TMin Tmin;
	@XmlElement
	private TMax Tmax;
	@XmlElement
	private PMin Pmin;
	@XmlElement
	private PMax Pmax;
	@XmlElement
	private RateCoeffFloatArray floatArray;

	public List<Arrhenius> getArrhenius() {
		return Arrhenius;
	}

	public void setArrhenius(List<Arrhenius> arrhenius) {
		Arrhenius = arrhenius;
	}

	public LandauTeller getLandauTeller() {
		return landauTeller;
	}

	public void setLandauTeller(LandauTeller landauTeller) {
		this.landauTeller = landauTeller;
	}

	public FallOff getFalloff() {
		return falloff;
	}

	public void setFalloff(FallOff falloff) {
		this.falloff = falloff;
	}

	public Efficiencies getEfficiencies() {
		return efficiencies;
	}

	public void setEfficiencies(Efficiencies efficiencies) {
		this.efficiencies = efficiencies;
	}

	public TMin getTmin() {
		return Tmin;
	}

	public void setTmin(TMin tmin) {
		Tmin = tmin;
	}

	public TMax getTmax() {
		return Tmax;
	}

	public void setTmax(TMax tmax) {
		Tmax = tmax;
	}

	public PMin getPmin() {
		return Pmin;
	}

	public void setPmin(PMin pmin) {
		Pmin = pmin;
	}

	public PMax getPmax() {
		return Pmax;
	}

	public void setPmax(PMax pmax) {
		Pmax = pmax;
	}

	public RateCoeffFloatArray getFloatArray() {
		return floatArray;
	}

	public void setFloatArray(RateCoeffFloatArray floatArray) {
		this.floatArray = floatArray;
	}
}
