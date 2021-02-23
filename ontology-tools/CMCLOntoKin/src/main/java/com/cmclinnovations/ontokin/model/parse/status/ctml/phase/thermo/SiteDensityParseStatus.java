package com.cmclinnovations.ontokin.model.parse.status.ctml.phase.thermo;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not the site density tag and its attribute have already 
 * been parsed.
 * 
 * @author msff2
 *
 */
public class SiteDensityParseStatus {
	boolean siteDensity = false;
	boolean units = false;
	public boolean isSiteDensity() {
		return siteDensity;
	}
	public void setSiteDensity(boolean siteDensity) {
		this.siteDensity = siteDensity;
	}
	public boolean isUnits() {
		return units;
	}
	public void setUnits(boolean units) {
		this.units = units;
	}
}
