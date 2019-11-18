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

	public List<NISTElementGeometry> getValue() {
		return value;
	}

	public void setValue(List<NISTElementGeometry> value) {
		this.value = value;
	}
}
