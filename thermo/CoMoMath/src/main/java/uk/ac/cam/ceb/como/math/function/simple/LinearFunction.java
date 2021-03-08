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
public class LinearFunction extends Function<Double, Double>{

    private double a = 0.0;
    private double b = 1.0;
    
    public LinearFunction(double a, double b) {
        this.a = a;
        this.b = b;
    }
    
    public double getA() {
        return a;
    }
    
    public double getB() {
        return b;
    }
    
    public void setA(double a) {
        this.a = a;
    }
    
    public void setB(double b) {
        this.b = b;
    }
    
    @Override
    public Double f(Double x, Object... additionalData) throws FunctionCalculationException {
        return x * getB() + getA();
    }
    
}
