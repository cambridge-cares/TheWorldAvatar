package com.cmclinnovations.ontokin.model.data.structure.ctml.phase;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.persistence.oxm.annotations.XmlPath;

import com.cmclinnovations.ontokin.model.data.structure.ctml.phase.thermo.SiteDensity;
@XmlAccessorType(XmlAccessType.FIELD)
public class Thermo {
	
	@XmlAttribute
	private String model;
	
	@XmlPath("node[@name='site_density']")	
	private SiteDensity site_density;

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	public SiteDensity getSite_density() {
		return site_density;
	}

	public void setSite_density(SiteDensity site_density) {
		this.site_density = site_density;
	}
}
