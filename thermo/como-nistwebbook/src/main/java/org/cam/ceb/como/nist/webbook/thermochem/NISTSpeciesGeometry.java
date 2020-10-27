package org.cam.ceb.como.nist.webbook.thermochem;

import java.util.ArrayList;
import java.util.List;

/**
 * A model define to encode the geometry of a chemical species.
 * 
 * @author msff2
 *
 */
public class NISTSpeciesGeometry {
	private List<NISTElementGeometry> value = new ArrayList<NISTElementGeometry>();
	// Represents the geometric coordinates as a plain string.
	private String string;
	// Represents all bonds between atoms as a plain string.
	private String atomicBondsString;
	public List<NISTElementGeometry> getValue() {
		return value;
	}

	public void setValue(List<NISTElementGeometry> value) {
		this.value = value;
	}

	public String getString() {
		return string;
	}

	public void setString(String string) {
		this.string = string;
	}

	public String getAtomicBondsString() {
		return atomicBondsString;
	}

	public void setAtomicBondsString(String atomicBondsString) {
		this.atomicBondsString = atomicBondsString;
	}
}
