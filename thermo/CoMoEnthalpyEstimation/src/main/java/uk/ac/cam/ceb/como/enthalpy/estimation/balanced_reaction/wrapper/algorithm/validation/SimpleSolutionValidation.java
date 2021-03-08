/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.wrapper.algorithm.validation;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;

/**
 *
 * @author pb556
 */
public class SimpleSolutionValidation implements SolutionValidation {

    protected double tolerance;
    
    public SimpleSolutionValidation(double tolerance) {
        this.tolerance = tolerance;
    }
    
    @Override
    public boolean isValid(ReactionList rList) {
        // checks if there are no huge outliers available
        boolean first = true;
        double value = 0.0;
        for (Reaction r : rList) {
            if (first) {
                first = false;
                value = r.calculateHf();
                continue;
            } else {
                if (r.calculateHf() < value + tolerance && r.calculateHf() > value - tolerance) {
                    continue;
                } else {
                    return false;
                }
            }
        }
        return true;
    }
    
}
