package org.cam.ceb.como.nist.webbook.thermochem;

import java.util.ArrayList;
import java.util.List;

/**
 * A model defined to represent the enthapy of reaction of the current</br> 
 * species.
 *  
 * @author msff2
 *
 */
public class NISTEnthalpyOfReaction {
	private List<NISTEnthalpy> enthalpy = new ArrayList<NISTEnthalpy>();
	private String reaction;
	public List<NISTEnthalpy> getEnthalpy() {
		return enthalpy;
	}
	public void setEnthalpy(List<NISTEnthalpy> enthalpy) {
		this.enthalpy = enthalpy;
	}
	public String getReaction() {
		return reaction;
	}
	public void setReaction(String reaction) {
		this.reaction = reaction;
	}
}
