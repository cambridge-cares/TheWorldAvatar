/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver;

import java.util.LinkedHashSet;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.variable.VariableSet;

/**
 *
 * @author pb556
 */
public interface LPFormat {
    
	
    public String getInputString(Species species, LinkedHashSet<Species> speciesSet, VariableSet vSet);
    
}
