package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable;

import org.apache.commons.lang.StringUtils;

/**
 * 
 * A representation of varible for the isodesmic problem. It contains the naming
 * convention for creating an input of lp_solve.
 * 
 * @author pb556
 * 
 */

public class Variable {

    public final String name;
    public final String absName;
    public Number value;

    public Variable(String name) {
    	
        this.name = name;
        this.absName = "abs" + StringUtils.capitalize(name);
    }

    public Variable(String name, Number value) {
    	
        this(name);
        this.value = value;
    }

    /**
     * 
     * @author nk510 (caresssd@hermes.cam.ac.uk)
     * Getter and setter methods generated.
     * 
     * @return
     * 
     */
    
	public Number getValue() {
		
		return value;
	}

	public void setValue(Number value) {
		
		this.value = value;
	}

	public String getName() {
		
		return name;
	}

	public String getAbsName() {
		
		return absName;
	}
}