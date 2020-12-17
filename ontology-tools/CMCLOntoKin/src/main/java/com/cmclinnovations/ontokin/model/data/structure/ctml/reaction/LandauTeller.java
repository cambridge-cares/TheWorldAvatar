package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

@XmlType(propOrder={"B", "C"})
@XmlAccessorType(XmlAccessType.FIELD)
public class LandauTeller {
	@XmlElement(name="B")
	private LandauTellerCoefficientB B;
	@XmlElement(name="C")
	private LandauTellerCoefficientC C;

	public LandauTellerCoefficientB getB() {
		return B;
	}

	public void setB(LandauTellerCoefficientB b) {
		B = b;
	}

	public LandauTellerCoefficientC getC() {
		return C;
	}

	public void setC(LandauTellerCoefficientC c) {
		C = c;
	}
}
