/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.math.function;

/**
 *
 * @author pb556
 */
public abstract class Function<T, Y> {
    
    public abstract T f(Y x, Object... additionalData) throws FunctionCalculationException;
    
}
