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
public class PolynomialFunction extends Function<Double, Double>{

    private Double c[] = null;
        
    public PolynomialFunction() {
    }
    
    public PolynomialFunction(Double[] c) {
        this.c = c;
    }
    
    public Double[] getCoefficients() {
        return c;
    }
    
    public void setCoefficients(Double[] c) {
        this.c = c;
    }
    
    @Override
    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
        if (getCoefficients() == null || getCoefficients().length == 0) {
            return Double.NaN;
        }
        double value = 0.0;
        for (int i = 0; i < c.length; i++) {
            value += c[i] * Math.pow(x, i);
        }
        return value;
    }
}