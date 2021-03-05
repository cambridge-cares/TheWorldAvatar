/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.wrapper.algorithm.validation;

import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;

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
