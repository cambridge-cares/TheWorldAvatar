package org.cam.ceb.como.nist.webbook.thermochem;
/**
 * A model defined to encode the dipole moment of the current chemical species.
 * 
 * @author msff2
 *
 */
public class NISTDipoleMoment {
	private String value;
	private String units;

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}
}
