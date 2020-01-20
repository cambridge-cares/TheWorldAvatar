/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm.validation;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public interface SolutionValidation {
    
    public boolean isValid(ReactionList rList);
    
}
