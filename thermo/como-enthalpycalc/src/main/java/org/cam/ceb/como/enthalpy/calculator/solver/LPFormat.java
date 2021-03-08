/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.solver;

import java.util.Set;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.variable.VariableSet;

/**
 *
 * @author pb556
 */
public interface LPFormat {
    
    public String getInputString(Species species, Set<Species> speciesSet, VariableSet vSet);
    
}
