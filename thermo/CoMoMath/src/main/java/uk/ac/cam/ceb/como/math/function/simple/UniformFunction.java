/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.function.simple;

import uk.ac.cam.ceb.como.math.function.Function;
import uk.ac.cam.ceb.como.math.function.FunctionCalculationException;

/**
 *
 * @author pb556
 */
public class UniformFunction extends Function<Double, Double>{

    private double factor = 1.0;
    
    public UniformFunction(double factor) {
        this.factor = factor;
    }
    
    public double getFactor() {
        return factor;
    }
    
    public void setFactor(double factor) {
        this.factor = factor;
    }
    
    @Override
    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
        return getFactor();
    }
    
}
